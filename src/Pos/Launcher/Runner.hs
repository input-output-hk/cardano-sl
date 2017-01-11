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
import           Control.Concurrent.STM.TVar (newTVar)
import           Control.Lens                (each, to, (%~), (^..), (^?), _head, _tail)
import           Control.Monad.Fix           (MonadFix)
import           Data.Default                (def)
import           Data.List                   (nub)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (Mockable, MonadMockable, Production (..),
                                              Throw, bracket, currentTime, fork,
                                              killThread, throw)
import           Network.Transport           (Transport, closeTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node                        (Listener, NodeAction (..), SendActions,
                                              hoistListenerAction, hoistSendActions, node)
import qualified STMContainers.Map           as SM
import           System.Random               (newStdGen)
import           System.Wlog                 (WithLogger, logDebug, logError, logInfo,
                                              logWarning, releaseAllHandlers,
                                              traverseLoggerConfig, usingLoggerName)
import           Universum                   hiding (bracket)

import           Pos.Binary                  ()
import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (BiP (..), SysStartRequest (..),
                                              allListeners, sysStartReqListener,
                                              sysStartReqListenerSlave,
                                              sysStartRespListener,
                                              sysStartRespListenerNode)
import           Pos.Communication.PeerState (runPeerStateHolder)
import           Pos.Constants               (defaultPeers, isDevelopment, runningMode)
import qualified Pos.Constants               as Const
import           Pos.Context                 (ContextHolder (..), NodeContext (..),
                                              runContextHolder)
import           Pos.Crypto                  (createProxySecretKey, toPublic)
import           Pos.DB                      (MonadDB (..), getTip, initNodeDBs,
                                              openNodeDBs, runDBHolder, _gStateDB)
import           Pos.DB.Misc                 (addProxySecretKey)
import           Pos.Delegation.Class        (runDelegationT)
import           Pos.Util.TimeWarp      (sec)
import           Pos.Genesis                 (genesisLeaders)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.DHT.Model            (MonadDHT (..), sendToNeighbors)
import           Pos.DHT.Real             (KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Ssc.Class               (SscConstraint, SscNodeContext, SscParams,
                                              sscCreateNodeContext, sscLoadGlobalState)
import           Pos.Ssc.Extra               (runSscHolder)
import           Pos.Statistics              (getNoStatsT, runStatsT')
import           Pos.Txp.Holder              (runTxpLDHolder)
import qualified Pos.Txp.Types.UtxoView      as UV
import           Pos.Types                   (Timestamp (Timestamp), timestampF)
import           Pos.Update.MemState         (runUSHolder)
import           Pos.Util                    (runWithRandomIntervals')
import           Pos.Util.UserSecret         (peekUserSecret, usKeys, writeUserSecret)
import           Pos.WorkMode                (ProductionMode, RawRealMode, ServiceMode,
                                              StatsMode)

data RealModeResources = RealModeResources
    { rmTransport :: Transport
    , rmDHT       :: KademliaDHTInstance
    }

----------------------------------------------------------------------------
-- Service node runners
----------------------------------------------------------------------------

-- | Runs node as time-slave inside IO monad.
runTimeSlaveReal :: RealModeResources -> BaseParams -> Production Timestamp
runTimeSlaveReal res bp = do
    mvar <- liftIO newEmptyMVar
    runServiceMode res bp (listeners mvar) $ \sendActions ->
      case runningMode of
         Const.Development -> do
           tId <- fork $
             runWithRandomIntervals' (sec 10) (sec 60) $ liftIO (tryReadMVar mvar) >>= \case
                 Nothing -> do
                    logInfo "Asking neighbors for system start"
                    sendToNeighbors sendActions SysStartRequest `catchAll`
                       \e -> logDebug $ sformat
                       ("Error sending SysStartRequest to neighbors: " % shown) e
                 Just _ -> fail "Close thread"
           t <- liftIO $ takeMVar mvar
           killThread tId
           t <$ logInfo (sformat ("[Time slave] adopted system start " % timestampF) t)
         Const.Production ts -> logWarning "Time slave launched in Production" $> ts
  where
    listeners mvar =
      if isDevelopment
         then [sysStartReqListenerSlave, sysStartRespListener mvar]
         else []

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
    -> [Listener BiP (RawRealMode ssc)]
    -> (SendActions BiP (RawRealMode ssc) -> RawRealMode ssc a)
    -> Production a
runRawRealMode res np@NodeParams {..} sscnp listeners action =
    usingLoggerName lpRunnerTag $ do
       initNC <- sscCreateNodeContext @ssc sscnp
       modernDBs <- openNodeDBs npRebuildDb npDbPathM
       -- FIXME: initialization logic must be in scenario.
       runDBHolder modernDBs . runCH np initNC $ initNodeDBs
       initTip <- runDBHolder modernDBs getTip
       initGS <- runDBHolder modernDBs (sscLoadGlobalState @ssc initTip)
       stateM <- liftIO SM.newIO
       runDBHolder modernDBs .
          runCH np initNC .
          flip runSscHolder initGS .
          runTxpLDHolder (UV.createFromDB . _gStateDB $ modernDBs) initTip .
          runDelegationT def .
          runUSHolder .
          runKademliaDHT (rmDHT res) .
          runPeerStateHolder stateM .
          runServer (rmTransport res) listeners $
              \sa -> nodeStartMsg npBaseParams >> action sa
  where
    LoggingParams {..} = bpLoggingParams npBaseParams

-- | ServiceMode runner.
runServiceMode
    :: RealModeResources
    -> BaseParams
    -> [Listener BiP ServiceMode]
    -> (SendActions BiP ServiceMode -> ServiceMode a)
    -> Production a
runServiceMode res bp@BaseParams{..} listeners action =
    usingLoggerName (lpRunnerTag bpLoggingParams) .
    runKademliaDHT (rmDHT res) .
    runServer (rmTransport res) listeners $
        \sa -> nodeStartMsg bp >> action sa

runServer :: (MonadIO m, MonadMockable m, MonadFix m)
  => Transport -> [Listener BiP m] -> (SendActions BiP m -> m b) -> m b
runServer transport listeners action = do
    stdGen <- liftIO newStdGen
    node (concrete transport) stdGen BiP $ \__node ->
        pure $ NodeAction listeners action

-- | ProductionMode runner.
runProductionMode
    :: forall ssc a.
       (SscConstraint ssc)
    => RealModeResources
    -> NodeParams
    -> SscParams ssc
    -> (SendActions BiP (ProductionMode ssc) -> ProductionMode ssc a)
    -> Production a
runProductionMode res np@NodeParams {..} sscnp action =
    runRawRealMode res np sscnp listeners $
        \sendActions -> getNoStatsT . action $ hoistSendActions lift getNoStatsT sendActions
  where
    listeners = addDevListeners npSystemStart commonListeners
    commonListeners = hoistListenerAction getNoStatsT lift <$> allListeners

-- | StatsMode runner.
-- [CSL-169]: spawn here additional listener, which would accept stat queries
-- can be done as part of refactoring (or someone who will refactor will create new issue).
runStatsMode
    :: forall ssc a.
       (SscConstraint ssc)
    => RealModeResources
    -> NodeParams
    -> SscParams ssc
    -> (SendActions BiP (StatsMode ssc) -> StatsMode ssc a)
    -> Production a
runStatsMode res np@NodeParams {..} sscnp action = do
    statMap <- liftIO SM.newIO
    let listeners = addDevListeners npSystemStart commonListeners
        commonListeners = hoistListenerAction (runStatsT' statMap) lift <$> allListeners
    runRawRealMode res np sscnp listeners $
        \sendActions -> do
            runStatsT' statMap . action $ hoistSendActions lift (runStatsT' statMap) sendActions

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH :: (MonadDB ssc m, MonadFail m)
      => NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH NodeParams {..} sscNodeContext act = do
    jlFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    semaphore <- liftIO newEmptyMVar
    lrcSync <- liftIO newEmptyMVar
    userSecret <- peekUserSecret npKeyfilePath

    -- Get primary secret key
    (primarySecretKey, userSecret') <- case npSecretKey of
        Nothing -> case userSecret ^? usKeys . _head of
            Nothing -> fail $ "No secret keys are found in " ++ npKeyfilePath
            Just sk -> return (sk, userSecret)
        Just sk -> do
            let us = userSecret & usKeys %~ (sk :) . filter (/= sk)
            writeUserSecret us
            return (sk, us)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey primarySecretKey) eternity . toPublic
        ownPSKs = userSecret' ^.. usKeys._tail.each.to makeOwnPSK
    forM_ ownPSKs addProxySecretKey

    userSecretVar <- liftIO . atomically . newTVar $ userSecret'
    let ctx =
            NodeContext
            { ncSystemStart = npSystemStart
            , ncSecretKey = primarySecretKey
            , ncGenesisUtxo = npCustomUtxo
            , ncGenesisLeaders = genesisLeaders npCustomUtxo
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
    :: Monad m => Timestamp
    -> [Listener BiP m]
    -> [Listener BiP m]
addDevListeners sysStart ls =
    if isDevelopment
    then sysStartRespListenerNode : sysStartReqListener sysStart : ls
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
        { kdcKeyOrType = bpDHTKeyOrType
        , kdcPort = bpPort
        , kdcInitialPeers = nub $ bpDHTPeers ++ defaultPeers
        , kdcExplicitInitial = bpDHTExplicitInitial
        }

createTransport :: (MonadIO m, WithLogger m, Mockable Throw m) => Word16 -> m Transport
createTransport port = do
    transportE <- liftIO $ TCP.createTransport
                             "0.0.0.0"
                             (show port)
                             TCP.defaultTCPParameters
    case transportE of
      Left e -> do
          logError $ sformat ("Error creating TCP transport: " % shown) e
          throw e
      Right transport -> return transport

bracketTransport :: BaseParams -> (Transport -> Production a) -> Production a
bracketTransport BaseParams{..} = bracket (withLog $ createTransport bpPort) (liftIO . closeTransport)
  where
    withLog = usingLoggerName $ lpRunnerTag bpLoggingParams

bracketResources :: BaseParams -> (RealModeResources -> Production a) -> IO a
bracketResources bp action =
    loggerBracket (bpLoggingParams bp) .
    runProduction .
    bracketDHTInstance bp $ \rmDHT ->
    bracketTransport bp $ \rmTransport ->
        action $ RealModeResources {..}
