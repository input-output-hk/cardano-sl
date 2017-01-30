{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRawRealMode
       , runProductionMode
       , runStatsMode
       , runServiceMode

       --  -- * Service runners
       , runTimeSlaveReal
       , runTimeLordReal

       -- * Exported for custom usage in CLI utils
       , addDevListeners
       , setupLoggers
       , bracketDHTInstance
       , runServer
       , loggerBracket
       , createTransport
       , bracketTransport
       , bracketResources
       , RealModeResources(..)
       ) where

import           Control.Concurrent.MVar     (newEmptyMVar, newMVar, takeMVar,
                                              tryReadMVar)
import           Control.Concurrent.STM      (newEmptyTMVarIO, newTBQueueIO, newTVarIO)
import           Control.Lens                (each, to, _tail)
import           Control.Monad.Fix           (MonadFix)
import qualified Data.ByteString.Char8       as BS8
import           Data.Default                (def)
import           Data.List                   (nub)
import           Data.Proxy                  (Proxy (..))
import           Data.Tagged                 (proxy)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (CurrentTime, Mockable, MonadMockable,
                                              Production (..), Throw, bracket,
                                              currentTime, delay, fork, killThread, throw)
import           Network.Transport           (Transport, closeTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node                        (NodeAction (..), hoistSendActions, node)
import qualified STMContainers.Map           as SM
import           System.Random               (newStdGen)
import           System.Wlog                 (WithLogger, logError, logInfo, logWarning,
                                              releaseAllHandlers, traverseLoggerConfig,
                                              usingLoggerName)
import           Universum                   hiding (bracket)

import           Pos.Binary                  ()
import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (ActionSpec (..), BiP (..),
                                              ConversationActions (..), InSpecs (..),
                                              ListenersWithOut, OutSpecs (..),
                                              PeerId (..), SysStartRequest (..),
                                              SysStartResponse, VerInfo (..),
                                              allListeners, allStubListeners,
                                              handleSysStartResp, hoistListenerSpec,
                                              mergeLs, stubListenerOneMsg,
                                              sysStartReqListener, sysStartRespListener,
                                              toAction, unpackLSpecs)
import           Pos.Communication.PeerState (runPeerStateHolder)
import           Pos.Constants               (lastKnownBlockVersion, protocolMagic)
import           Pos.Constants               (blockRetrievalQueueSize,
                                              networkConnectionTimeout)
import qualified Pos.Constants               as Const
import           Pos.Context                 (ContextHolder (..), NodeContext (..),
                                              runContextHolder)
import           Pos.Crypto                  (createProxySecretKey, toPublic)
import           Pos.DB                      (MonadDB (..), getTip, initNodeDBs,
                                              openNodeDBs, runDBHolder, _gStateDB)
import qualified Pos.DB.GState               as GState
import           Pos.DB.Misc                 (addProxySecretKey)
import           Pos.Delegation.Holder       (runDelegationT)
import           Pos.DHT.Model               (MonadDHT (..), converseToNeighbors,
                                              getMeaningPart)
import           Pos.DHT.Real                (KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Genesis                 (genesisLeaders)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Slotting                (SlottingState (..))
import           Pos.Ssc.Class               (SscConstraint, SscNodeContext, SscParams,
                                              sscCreateNodeContext, sscLoadGlobalState)
import           Pos.Ssc.Class.Listeners     (SscListenersClass)
import           Pos.Ssc.Extra               (runSscHolder)
import           Pos.Statistics              (getNoStatsT, runStatsT')
import           Pos.Txp.Holder              (runTxpLDHolder)
import qualified Pos.Txp.Types.UtxoView      as UV
import           Pos.Types                   (Timestamp (Timestamp), timestampF,
                                              unflattenSlotId)
import           Pos.Update.MemState         (runUSHolder)
import           Pos.Util                    (mappendPair, runWithRandomIntervalsNow)
import           Pos.Util.TimeWarp           (sec)
import           Pos.Util.UserSecret         (usKeys)
import           Pos.WorkMode                (MinWorkMode, ProductionMode, RawRealMode,
                                              ServiceMode, StatsMode)
data RealModeResources = RealModeResources
    { rmTransport :: Transport
    , rmDHT       :: KademliaDHTInstance
    }

----------------------------------------------------------------------------
-- Service node runners
----------------------------------------------------------------------------

-- | Runs node as time-slave inside IO monad.
runTimeSlaveReal
    :: SscListenersClass ssc
    => Proxy ssc -> RealModeResources -> BaseParams -> Production Timestamp
runTimeSlaveReal sscProxy res bp = do
    mvar <- liftIO newEmptyMVar
    runServiceMode res bp (listeners mvar) sysStartOuts . toAction $ \sendActions ->
      case Const.isDevelopment of
         True -> do
           tId <- fork $ do
             delay (sec 5)
             runWithRandomIntervalsNow (sec 10) (sec 60) $ liftIO (tryReadMVar mvar) >>= \case
                 Nothing -> do
                    logInfo "Asking neighbors for system start"
                    converseToNeighbors sendActions $ \peerId conv -> do
                        send conv SysStartRequest
                        mResp <- recv conv
                        whenJust mResp $ handleSysStartResp' mvar peerId sendActions
                 Just _ -> fail "Close thread"
           t <- liftIO $ takeMVar mvar
           killThread tId
           t <$ logInfo (sformat ("[Time slave] adopted system start " % timestampF) t)
         False -> logWarning "Time slave launched in Production" $>
           panic "Time slave in production, rly?"
  where
    (handleSysStartResp', sysStartOuts) = handleSysStartResp
    listeners mvar =
      if Const.isDevelopment
         then second (`mappend` sysStartOuts) $
                proxy allStubListeners sscProxy `mappendPair`
                  mergeLs [ stubListenerOneMsg (Proxy :: Proxy SysStartRequest)
                          , sysStartRespListener mvar
                          ]
         else proxy allStubListeners sscProxy

-- | Runs time-lord to acquire system start.
runTimeLordReal :: LoggingParams -> Production Timestamp
runTimeLordReal LoggingParams{..} = do
    t <- Timestamp <$> currentTime
    usingLoggerName lpRunnerTag (doLog t) $> t
  where
    doLog t = do
        realTime <- liftIO Time.getZonedTime
        logInfo (sformat ("[Time lord] System start: " %timestampF%", i. e.: "%shown) t realTime)

------------------------------------------------------------------------------
---- High level runners
------------------------------------------------------------------------------

-- | RawRealMode runner.
runRawRealMode
    :: forall ssc a.
       SscConstraint ssc
    => RealModeResources
    -> NodeParams
    -> SscParams ssc
    -> ListenersWithOut (RawRealMode ssc)
    -> OutSpecs
    -> ActionSpec (RawRealMode ssc) a
    -> Production a
runRawRealMode res np@NodeParams {..} sscnp listeners outSpecs (ActionSpec action) =
    usingLoggerName lpRunnerTag $ do
       initNC <- sscCreateNodeContext @ssc sscnp
       modernDBs <- openNodeDBs npRebuildDb npDbPathM
       -- FIXME: initialization logic must be in scenario.
       runDBHolder modernDBs . runCH np initNC $ initNodeDBs
       initTip <- runDBHolder modernDBs getTip
       initGS <- runDBHolder modernDBs (sscLoadGlobalState @ssc)
       stateM <- liftIO SM.newIO
       runDBHolder modernDBs .
          runCH np initNC .
          flip runSscHolder initGS .
          runTxpLDHolder (UV.createFromDB . _gStateDB $ modernDBs) initTip .
          runDelegationT def .
          runUSHolder .
          runKademliaDHT (rmDHT res) .
          runPeerStateHolder stateM .
          runServer (rmTransport res) listeners outSpecs . ActionSpec $
              \vI sa -> nodeStartMsg npBaseParams >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams npBaseParams

-- | ServiceMode runner.
runServiceMode
    :: RealModeResources
    -> BaseParams
    -> ListenersWithOut ServiceMode
    -> OutSpecs
    -> ActionSpec ServiceMode a
    -> Production a
runServiceMode res bp@BaseParams{..} listeners outSpecs (ActionSpec action) =
    usingLoggerName (lpRunnerTag bpLoggingParams) .
    runKademliaDHT (rmDHT res) .
    runServer (rmTransport res) listeners outSpecs . ActionSpec $
        \vI sa -> nodeStartMsg bp >> action vI sa

runServer :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m, MonadDHT m)
  => Transport
  -> ListenersWithOut m
  -> OutSpecs
  -> ActionSpec m b
  -> m b
runServer transport packedLS (OutSpecs wouts) (ActionSpec action) = do
    ourPeerId <- PeerId . getMeaningPart <$> currentNodeKey
    let (listeners', InSpecs ins, OutSpecs outs) = unpackLSpecs packedLS
        ourVerInfo = VerInfo protocolMagic lastKnownBlockVersion ins $ outs <> wouts
        listeners = listeners' ourVerInfo
    stdGen <- liftIO newStdGen
    node (concrete transport) stdGen BiP (ourPeerId, ourVerInfo) $ \__node ->
        pure $ NodeAction listeners (action ourVerInfo)

-- | ProductionMode runner.
runProductionMode
    :: forall ssc a.
       (SscConstraint ssc)
    => RealModeResources
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (ProductionMode ssc) a, OutSpecs)
    -> Production a
runProductionMode res np@NodeParams {..} sscnp (ActionSpec action, outSpecs) =
    runRawRealMode res np sscnp listeners outSpecs . ActionSpec $
        \vI sendActions -> getNoStatsT . action vI $ hoistSendActions lift getNoStatsT sendActions
  where
    listeners = addDevListeners npSystemStart commonListeners
    commonListeners = first (hoistListenerSpec getNoStatsT lift <$>) allListeners

-- | StatsMode runner.
-- [CSL-169]: spawn here additional listener, which would accept stat queries
-- can be done as part of refactoring (or someone who will refactor will create new issue).
runStatsMode
    :: forall ssc a.
       (SscConstraint ssc)
    => RealModeResources
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (StatsMode ssc) a, OutSpecs)
    -> Production a
runStatsMode res np@NodeParams {..} sscnp (ActionSpec action, outSpecs) = do
    statMap <- liftIO SM.newIO
    let listeners = addDevListeners npSystemStart commonListeners
        commonListeners = first (hoistListenerSpec (runStatsT' statMap) lift <$>) allListeners
    runRawRealMode res np sscnp listeners outSpecs . ActionSpec $
        \vI sendActions -> do
            runStatsT' statMap . action vI $ hoistSendActions lift (runStatsT' statMap) sendActions

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH :: (MonadDB ssc m, Mockable CurrentTime m)
      => NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH NodeParams {..} sscNodeContext act = do
    jlFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    semaphore <- liftIO newEmptyMVar
    updSemaphore <- liftIO newEmptyMVar
    lrcSync <- liftIO $ newTVarIO (True, 0)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey npSecretKey) eternity . toPublic
        ownPSKs = npUserSecret ^.. usKeys._tail.each.to makeOwnPSK
    forM_ ownPSKs addProxySecretKey

    userSecretVar <- liftIO . newTVarIO $ npUserSecret
    queue <- liftIO $ newTBQueueIO blockRetrievalQueueSize
    recoveryHeaderVar <- liftIO newEmptyTMVarIO
    slottingStateVar <- do
        ssSlotDuration <- GState.getSlotDuration
        ssNtpData <- (0,) <$> currentTime
        -- current time isn't quite validly, but it doesn't matter
        let ssNtpLastSlot = unflattenSlotId 0
        liftIO $ newTVarIO SlottingState{..}
    let ctx =
            NodeContext
            { ncSystemStart = npSystemStart
            , ncSecretKey = npSecretKey
            , ncGenesisUtxo = npCustomUtxo
            , ncGenesisLeaders = genesisLeaders npCustomUtxo
            , ncSlottingState = slottingStateVar
            , ncTimeLord = npTimeLord
            , ncJLFile = jlFile
            , ncDbPath = npDbPathM
            , ncSscContext = sscNodeContext
            , ncAttackTypes = npAttackTypes
            , ncAttackTargets = npAttackTargets
            , ncPropagation = npPropagation
            , ncBlkSemaphore = semaphore
            , ncLrcSync = lrcSync
            , ncUserSecret = userSecretVar
            , ncKademliaDump = bpKademliaDump npBaseParams
            , ncBlockRetrievalQueue = queue
            , ncRecoveryHeader = recoveryHeaderVar
            , ncUpdateSemaphore = updSemaphore
            , ncUpdatePath = npUpdatePath
            , ncUpdateWithPkg = npUpdateWithPkg
            }
    runContextHolder ctx act

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: WithLogger m => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where
    msg = sformat ("Started node, joining to DHT network " %build) bpDHTPeers

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers LoggingParams{..} = do
    lpLoggerConfig <- readLoggerConfig lpConfigPath
    traverseLoggerConfig dhtMapper lpLoggerConfig lpHandlerPrefix
  where
    dhtMapper  name | name == "dht"  = dhtLoggerName (Proxy :: Proxy (RawRealMode ssc))
                    | otherwise      = name

-- | RAII for node starter.
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) releaseAllHandlers

addDevListeners
    :: MinWorkMode m => Timestamp
    -> ListenersWithOut m
    -> ListenersWithOut m
addDevListeners sysStart ls =
    if Const.isDevelopment
    then mergeLs [ stubListenerOneMsg (Proxy :: Proxy SysStartResponse)
                 , sysStartReqListener sysStart] `mappendPair` ls
    else ls

bracketDHTInstance
    :: BaseParams -> (KademliaDHTInstance -> Production a) -> Production a
bracketDHTInstance BaseParams {..} action = bracket acquire release action
  where
    withLog = usingLoggerName $ lpRunnerTag bpLoggingParams
    acquire = withLog $ startDHTInstance instConfig
    release = withLog . stopDHTInstance
    instConfig =
        KademliaDHTInstanceConfig
        { kdcKey = bpDHTKey
        , kdcPort = snd bpIpPort
        , kdcInitialPeers = nub $ bpDHTPeers ++ Const.defaultPeers
        , kdcExplicitInitial = bpDHTExplicitInitial
        , kdcDumpPath = bpKademliaDump
        }

createTransport :: (MonadIO m, WithLogger m, Mockable Throw m) => String -> Word16 -> m Transport
createTransport ip port = do
    transportE <- liftIO $ TCP.createTransport
                             "0.0.0.0"
                             ip
                             (show port)
                             (TCP.defaultTCPParameters { TCP.transportConnectTimeout = Just $ fromIntegral networkConnectionTimeout })
    case transportE of
      Left e -> do
          logError $ sformat ("Error creating TCP transport: " % shown) e
          throw e
      Right transport -> return transport

bracketTransport :: BaseParams -> (Transport -> Production a) -> Production a
bracketTransport BaseParams{..} = bracket (withLog $ createTransport (BS8.unpack $ fst bpIpPort) (snd bpIpPort)) (liftIO . closeTransport)
  where
    withLog = usingLoggerName $ lpRunnerTag bpLoggingParams

bracketResources :: BaseParams -> (RealModeResources -> Production a) -> IO a
bracketResources bp action =
    loggerBracket (bpLoggingParams bp) .
    runProduction .
    bracketDHTInstance bp $ \rmDHT ->
    bracketTransport bp $ \rmTransport ->
        action $ RealModeResources {..}
