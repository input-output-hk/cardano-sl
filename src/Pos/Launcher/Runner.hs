{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
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
       , runServer_
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
                                              currentTime, delay, finally, fork,
                                              killThread, throw)
import           Network.Transport           (Transport, closeTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node                        (Node, NodeAction (..), hoistSendActions,
                                              node)
import           Node.Util.Monitor           (setupMonitor, stopMonitor)
import qualified STMContainers.Map           as SM
import           System.Random               (newStdGen)
import           System.Wlog                 (LoggerConfig (..), WithLogger, logError,
                                              logInfo, logWarning, mapperB, memoryB,
                                              productionB, releaseAllHandlers,
                                              setupLogging, usingLoggerName)
import           Universum                   hiding (bracket, finally)

import           Pos.Binary                  ()
import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (ActionSpec (..), BiP (..),
                                              ConversationActions (..), InSpecs (..),
                                              ListenersWithOut, OutSpecs (..),
                                              PeerId (..), SysStartRequest (..),
                                              SysStartResponse, VerInfo (..),
                                              allListeners, allStubListeners, convH,
                                              handleSysStartResp, hoistListenerSpec,
                                              mergeLs, protocolListeners,
                                              stubListenerConv, stubListenerOneMsg,
                                              sysStartReqListener, sysStartRespListener,
                                              toAction, toOutSpecs, unpackLSpecs)
import           Pos.Communication.PeerState (runPeerStateHolder)
import           Pos.Constants               (lastKnownBlockVersion, protocolMagic)
import           Pos.Constants               (blockRetrievalQueueSize,
                                              networkConnectionTimeout,
                                              propagationQueueSize)
import qualified Pos.Constants               as Const
import           Pos.Context                 (ContextHolder (..), NodeContext (..),
                                              runContextHolder)
import           Pos.Crypto                  (createProxySecretKey, toPublic)
import           Pos.DB                      (MonadDB (..), getTip, initNodeDBs,
                                              openNodeDBs, runDBHolder, _gStateDB)
import qualified Pos.DB.Lrc                  as LrcDB
import           Pos.DB.Misc                 (addProxySecretKey)
import           Pos.Delegation.Holder       (runDelegationT)
import           Pos.DHT.Model               (MonadDHT (..), converseToNeighbors,
                                              getMeaningPart)
import           Pos.DHT.Real                (KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Slotting                (mkNtpSlottingVar, mkSlottingVar,
                                              runNtpSlotting, runSlottingHolder)
import           Pos.Ssc.Class               (SscConstraint, SscHelpersClass,
                                              SscListenersClass, SscNodeContext,
                                              SscParams, sscCreateNodeContext)
import           Pos.Ssc.Extra               (ignoreSscHolder, mkStateAndRunSscHolder)
import           Pos.Statistics              (getNoStatsT, runStatsT')
import           Pos.Txp.Holder              (runTxpLDHolder)
import qualified Pos.Txp.Types.UtxoView      as UV
import           Pos.Types                   (Timestamp (Timestamp), timestampF)
import           Pos.Update.MemState         (runUSHolder)
import           Pos.Util                    (mappendPair, runWithRandomIntervalsNow)
import           Pos.Util.TimeWarp           (sec)
import           Pos.Util.UserSecret         (usKeys)
import           Pos.Worker                  (allWorkersCount)
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
    :: (SscListenersClass ssc, SscHelpersClass ssc)
    => Proxy ssc -> RealModeResources -> BaseParams -> Production Timestamp
runTimeSlaveReal sscProxy res bp = do
    mvar <- liftIO newEmptyMVar
    runServiceMode res bp (listeners mvar) outs . toAction $ \sendActions ->
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
    outs = sysStartOuts
              <> toOutSpecs [ convH (Proxy :: Proxy SysStartRequest)
                                    (Proxy :: Proxy SysStartResponse)]
    (handleSysStartResp', sysStartOuts) = handleSysStartResp
    listeners mvar =
      if Const.isDevelopment
         then second (`mappend` sysStartOuts) $
                proxy allStubListeners sscProxy `mappendPair`
                  mergeLs [ stubListenerConv (Proxy :: Proxy (SysStartRequest, SysStartResponse))
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

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

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
       -- TODO [CSL-775] ideally initialization logic should be in scenario.
       runDBHolder modernDBs . runCH np initNC $ initNodeDBs
       initTip <- runDBHolder modernDBs getTip
       stateM <- liftIO SM.newIO
       stateM_ <- liftIO SM.newIO
       slottingVar <- runDBHolder modernDBs mkSlottingVar
       ntpSlottingVar <- mkNtpSlottingVar

       -- TODO [CSL-775] need an effect-free way of running this into IO.
       let runIO :: forall t . RawRealMode ssc t -> IO t
           runIO = runProduction .
                       usingLoggerName lpRunnerTag .
                       runDBHolder modernDBs .
                       runCH np initNC .
                       runSlottingHolder slottingVar .
                       runNtpSlotting ntpSlottingVar .
                       ignoreSscHolder .
                       runTxpLDHolder (UV.createFromDB . _gStateDB $ modernDBs) initTip .
                       runDelegationT def .
                       runUSHolder .
                       runKademliaDHT (rmDHT res) .
                       runPeerStateHolder stateM_

       let startMonitoring node' = case lpEkgPort of
               Nothing   -> return Nothing
               Just port -> Just <$> setupMonitor port runIO node'

       let stopMonitoring it = case it of
               Nothing        -> return ()
               Just ekgServer -> stopMonitor ekgServer

       runDBHolder modernDBs .
          runCH np initNC .
          runSlottingHolder slottingVar .
          runNtpSlotting ntpSlottingVar .
          (mkStateAndRunSscHolder @ssc) .
          runTxpLDHolder (UV.createFromDB . _gStateDB $ modernDBs) initTip .
          runDelegationT def .
          runUSHolder .
          runKademliaDHT (rmDHT res) .
          runPeerStateHolder stateM .
          runServer (rmTransport res) listeners outSpecs startMonitoring stopMonitoring . ActionSpec $
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
runServiceMode res bp@BaseParams {..} listeners outSpecs (ActionSpec action) = do
    stateM <- liftIO SM.newIO
    usingLoggerName (lpRunnerTag bpLoggingParams) .
        runKademliaDHT (rmDHT res) .
        runPeerStateHolder stateM .
        runServer_ (rmTransport res) listeners outSpecs . ActionSpec $ \vI sa ->
        nodeStartMsg bp >> action vI sa

runServer
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m, MonadDHT m)
    => Transport
    -> ListenersWithOut m
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer transport packedLS (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    ourPeerId <- PeerId . getMeaningPart <$> currentNodeKey
    let (listeners', InSpecs ins, OutSpecs outs) = unpackLSpecs packedLS
        ourVerInfo =
            VerInfo protocolMagic lastKnownBlockVersion ins $ outs <> wouts
        listeners = listeners' ourVerInfo ++ protocolListeners
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo "%build) ourVerInfo
    node (concrete transport) stdGen BiP (ourPeerId, ourVerInfo) $ \__node ->
        pure $
        NodeAction listeners $ \sendActions -> do
            t <- withNode __node
            a <- action ourVerInfo sendActions `finally` afterNode t
            return a

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m, MonadDHT m)
    => Transport -> ListenersWithOut m -> OutSpecs -> ActionSpec m b -> m b
runServer_ transport packedLS outSpecs =
    runServer transport packedLS outSpecs acquire release
  where
    acquire = const pass
    release = const pass

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

runCH :: forall ssc m a . (SscConstraint ssc, MonadDB ssc m, Mockable CurrentTime m)
      => NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH params@NodeParams {..} sscNodeContext act = do
    logCfg <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    jlFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    semaphore <- liftIO newEmptyMVar
    updSemaphore <- liftIO newEmptyMVar

    -- TODO [CSL-775] lrc initialization logic is duplicated.
    lrcSync <- liftIO . newTVarIO . (True,) =<< LrcDB.getEpoch

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey npSecretKey) eternity . toPublic
        ownPSKs = npUserSecret ^.. usKeys._tail.each.to makeOwnPSK
    forM_ ownPSKs addProxySecretKey

    userSecretVar <- liftIO . newTVarIO $ npUserSecret
    queue <- liftIO $ newTBQueueIO blockRetrievalQueueSize
    propQueue <- liftIO $ newTBQueueIO propagationQueueSize
    recoveryHeaderVar <- liftIO newEmptyTMVarIO
    shutdownFlag <- liftIO $ newTVarIO False
    shutdownQueue <- liftIO $ newTBQueueIO allWorkersCount
    curTime <- liftIO Time.getCurrentTime
    let ctx =
            NodeContext
            { ncJLFile = jlFile
            , ncSscContext = sscNodeContext
            , ncBlkSemaphore = semaphore
            , ncLrcSync = lrcSync
            , ncUserSecret = userSecretVar
            , ncBlockRetrievalQueue = queue
            , ncInvPropagationQueue = propQueue
            , ncRecoveryHeader = recoveryHeaderVar
            , ncUpdateSemaphore = updSemaphore
            , ncShutdownFlag = shutdownFlag
            , ncShutdownNotifyQueue = shutdownQueue
            , ncNodeParams = params
            , ncLoggerConfig = logCfg
            , ncSendLock = Nothing
            , ncStartTime = curTime
            }
    runContextHolder ctx act

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: WithLogger m => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where
    msg = sformat ("Started node, joining to DHT network " %build) bpDHTPeers

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    -- TODO: introduce Maybe FilePath builder for filePrefix
    let cfgBuilder = productionB <>
                     memoryB (1024 * 1024 * 5) <> -- ~5 mb
                     mapperB dhtMapper <>
                     (mempty { _lcFilePrefix = lpHandlerPrefix })
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder
  where
    dhtMapper name | name == "dht" = dhtLoggerName (Proxy :: Proxy (RawRealMode ssc))
                   | otherwise     = name

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers params = setupLogging =<< getRealLoggerConfig params

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
    --withLog = usingLoggerName $ lpRunnerTag bpLoggingParams
    acquire = usingLoggerName (lpRunnerTag bpLoggingParams) (startDHTInstance instConfig)
    release = usingLoggerName (lpRunnerTag bpLoggingParams) . stopDHTInstance
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
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral networkConnectionTimeout
             })
    transportE <-
        liftIO $ TCP.createTransport "0.0.0.0" ip (show port) tcpParams
    case transportE of
        Left e -> do
            logError $ sformat ("Error creating TCP transport: " % shown) e
            throw e
        Right transport -> return transport

bracketTransport :: BaseParams -> (Transport -> Production a) -> Production a
bracketTransport BaseParams {..} =
    bracket
        (withLog $ createTransport (BS8.unpack $ fst bpIpPort) (snd bpIpPort))
        (liftIO . closeTransport)
  where
    withLog = usingLoggerName $ lpRunnerTag bpLoggingParams

bracketResources :: BaseParams -> (RealModeResources -> Production a) -> IO a
bracketResources bp action =
    loggerBracket (bpLoggingParams bp) .
    runProduction .
    bracketDHTInstance bp $ \rmDHT ->
    bracketTransport bp $ \rmTransport ->
        action $ RealModeResources {..}
