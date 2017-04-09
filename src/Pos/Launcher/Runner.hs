{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRawRealMode
       , runProductionMode
       , runStatsMode
       , runServiceMode

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

import           Control.Concurrent.STM      (newEmptyTMVarIO, newTBQueueIO)
import           Control.Lens                (each, to, _tail)
import           Control.Monad.Fix           (MonadFix)
import qualified Data.ByteString.Char8       as BS8
import           Data.Default                (def)
import           Data.Tagged                 (untag)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (CurrentTime, Mockable, MonadMockable,
                                              Production (..), Throw, bracket, finally,
                                              throw)
import           Network.QDisc.Fair          (fairQDisc)
import           Network.Transport           (Transport, closeTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node                        (Node, NodeAction (..),
                                              defaultNodeEnvironment, hoistSendActions,
                                              node, simpleNodeEndPoint)
import           Node.Util.Monitor           (setupMonitor, stopMonitor)
import qualified STMContainers.Map           as SM
import           System.Random               (newStdGen)
import           System.Wlog                 (LoggerConfig (..), WithLogger, logError,
                                              logInfo, mapperB, productionB,
                                              releaseAllHandlers, setupLogging,
                                              usingLoggerName)
import           Universum                   hiding (bracket, finally)

import           Pos.Binary                  ()
import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (ActionSpec (..), BiP (..), InSpecs (..),
                                              ListenersWithOut, OutSpecs (..),
                                              PeerId (..), SysStartResponse, VerInfo (..),
                                              allListeners, hoistListenerSpec, mergeLs,
                                              stubListenerOneMsg, sysStartReqListener,
                                              unpackLSpecs)
import           Pos.Communication.PeerState (runPeerStateHolder)
import qualified Pos.Constants               as Const
import           Pos.Context                 (ContextHolder (..), NodeContext (..),
                                              runContextHolder)
import           Pos.Core                    (Timestamp)
import           Pos.Crypto                  (createProxySecretKey, encToPublic)
import           Pos.DB                      (MonadDB (..), runDBHolder)
import           Pos.DB.DB                   (initNodeDBs, openNodeDBs)
import           Pos.DB.GState               (getTip)
import           Pos.DB.Misc                 (addProxySecretKey)
import           Pos.Delegation.Holder       (runDelegationT)
import           Pos.DHT.Model               (MonadDHT (..), getMeaningPart)
import           Pos.DHT.Real                (KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              runKademliaDHT, startDHTInstance,
                                              stopDHTInstance)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Lrc.Context             (LrcContext (..), LrcSyncData (..))
import qualified Pos.Lrc.DB                  as LrcDB
import           Pos.Slotting                (SlottingVar, mkNtpSlottingVar,
                                              runNtpSlotting, runSlottingHolder)
import           Pos.Ssc.Class               (SscConstraint, SscNodeContext, SscParams,
                                              sscCreateNodeContext)
import           Pos.Ssc.Extra               (ignoreSscHolder, mkStateAndRunSscHolder)
import           Pos.Statistics              (getNoStatsT, runStatsT')
import           Pos.Txp                     (mkTxpLocalData, runTxpHolder)
#ifdef WITH_EXPLORER
import           Pos.Explorer                (explorerTxpGlobalSettings)
#else
import           Pos.Txp                     (txpGlobalSettings)
#endif
import           Pos.Update.Context          (UpdateContext (..))
import qualified Pos.Update.DB               as GState
import           Pos.Update.MemState         (newMemVar)
import           Pos.Util                    (mappendPair)
import           Pos.Util.UserSecret         (usKeys)
import           Pos.Worker                  (allWorkersCount)
import           Pos.WorkMode                (MinWorkMode, ProductionMode, RawRealMode,
                                              ServiceMode, StatsMode)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

data RealModeResources = RealModeResources
    { rmTransport :: Transport
    , rmDHT       :: KademliaDHTInstance
    }

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
    -> RawRealMode ssc (ListenersWithOut (RawRealMode ssc))
    -> OutSpecs
    -> ActionSpec (RawRealMode ssc) a
    -> Production a
runRawRealMode res np@NodeParams {..} sscnp listeners outSpecs (ActionSpec action) =
    usingLoggerName lpRunnerTag $ do
       initNC <- untag @ssc sscCreateNodeContext sscnp
       modernDBs <- openNodeDBs npRebuildDb npDbPathM
       let allWorkersNum = allWorkersCount @ssc @(ProductionMode ssc)
       -- TODO [CSL-775] ideally initialization logic should be in scenario.
       runDBHolder modernDBs . runCH @ssc allWorkersNum np initNC $ initNodeDBs
       initTip <- runDBHolder modernDBs getTip
       stateM <- liftIO SM.newIO
       stateM_ <- liftIO SM.newIO
       slottingVar <- runDBHolder  modernDBs $ mkSlottingVar npSystemStart
       txpVar <- mkTxpLocalData mempty initTip
       ntpSlottingVar <- mkNtpSlottingVar

       -- TODO [CSL-775] need an effect-free way of running this into IO.
       let runIO :: forall t . RawRealMode ssc t -> IO t
           runIO = runProduction .
                       usingLoggerName lpRunnerTag .
                       runDBHolder modernDBs .
                       runCH @ssc allWorkersNum np initNC .
                       runSlottingHolder slottingVar .
                       runNtpSlotting ntpSlottingVar .
                       ignoreSscHolder .
                       runTxpHolder txpVar .
                       runDelegationT def .
                       runKademliaDHT (rmDHT res) .
                       runPeerStateHolder stateM_

       let startMonitoring node' = case lpEkgPort of
               Nothing   -> return Nothing
               Just port -> Just <$> setupMonitor port runIO node'

       let stopMonitoring it = whenJust it stopMonitor

       runDBHolder modernDBs .
          runCH allWorkersNum np initNC .
          runSlottingHolder slottingVar .
          runNtpSlotting ntpSlottingVar .
          (mkStateAndRunSscHolder @ssc) .
          runTxpHolder txpVar .
          runDelegationT def .
          runKademliaDHT (rmDHT res) .
          runPeerStateHolder stateM .
          runServer (rmTransport res) listeners outSpecs startMonitoring stopMonitoring . ActionSpec $
              \vI sa -> nodeStartMsg npBaseParams >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams npBaseParams

-- | Create new 'SlottingVar' using data from DB.
mkSlottingVar :: MonadDB m => Timestamp -> m SlottingVar
mkSlottingVar sysStart = do
    sd <- GState.getSlottingData
    (sysStart, ) <$> liftIO (newTVarIO sd)

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
    -> m (ListenersWithOut m)
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer transport packedLS_M (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    ourPeerId <- PeerId . getMeaningPart <$> currentNodeKey
    packedLS  <- packedLS_M
    let (listeners', InSpecs ins, OutSpecs outs) = unpackLSpecs packedLS
        ourVerInfo =
            VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
        listeners = listeners' ourVerInfo
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo "%build) ourVerInfo
    node (simpleNodeEndPoint (concrete transport)) stdGen BiP (ourPeerId, ourVerInfo) defaultNodeEnvironment $ \__node ->
        NodeAction listeners $ \sendActions -> do
            t <- withNode __node
            action ourVerInfo sendActions `finally` afterNode t

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m, MonadDHT m)
    => Transport -> ListenersWithOut m -> OutSpecs -> ActionSpec m b -> m b
runServer_ transport packedLS outSpecs =
    runServer transport (pure packedLS) outSpecs acquire release
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
    listeners = addDevListeners npSystemStart <$> commonListeners
    commonListeners = getNoStatsT $
        first (hoistListenerSpec getNoStatsT lift <$>) <$> allListeners

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
    let listeners = addDevListeners npSystemStart <$> commonListeners
        commonListeners = runStatsT' statMap $
            first (hoistListenerSpec (runStatsT' statMap) lift <$>) <$> allListeners
    runRawRealMode res np sscnp listeners outSpecs . ActionSpec $
        \vI sendActions -> do
            runStatsT' statMap . action vI $ hoistSendActions lift (runStatsT' statMap) sendActions

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH :: forall ssc m a . (SscConstraint ssc, MonadDB m, Mockable CurrentTime m)
      => Int -> NodeParams -> SscNodeContext ssc -> ContextHolder ssc m a -> m a
runCH allWorkersNum params@NodeParams {..} sscNodeContext act = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncJLFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    ncBlkSemaphore <- liftIO newEmptyMVar
    ucUpdateSemaphore <- liftIO newEmptyMVar

    -- TODO [CSL-775] lrc initialization logic is duplicated.
    epochDef <- LrcDB.getEpochDefault
    lcLrcSync <- liftIO $ newTVarIO (LrcSyncData True epochDef)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey npSecretKey) eternity . encToPublic
        ownPSKs = npUserSecret ^.. usKeys._tail.each.to makeOwnPSK
    forM_ ownPSKs addProxySecretKey

    ncUserSecret <- liftIO . newTVarIO $ npUserSecret
    ncBlockRetrievalQueue <- liftIO $
        newTBQueueIO Const.blockRetrievalQueueSize
    ncInvPropagationQueue <- liftIO $
        newTBQueueIO Const.propagationQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    ncProgressHeader <- liftIO newEmptyTMVarIO
    ncShutdownFlag <- liftIO $ newTVarIO False
    ncShutdownNotifyQueue <- liftIO $ newTBQueueIO allWorkersNum
    ncStartTime <- liftIO Time.getCurrentTime
    ncLastKnownHeader <- liftIO $ newTVarIO Nothing
    ucMemState <- newMemVar
    let ctx =
            NodeContext
            { ncSscContext = sscNodeContext
            , ncLrcContext = LrcContext {..}
            , ncUpdateContext = UpdateContext {..}
            , ncNodeParams = params
            , ncSendLock = Nothing
#ifdef WITH_EXPLORER
            , ncTxpGlobalSettings = explorerTxpGlobalSettings
#else
            , ncTxpGlobalSettings = txpGlobalSettings
#endif
            , .. }
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
        , kdcInitialPeers = ordNub $ bpDHTPeers ++ Const.defaultPeers
        , kdcExplicitInitial = bpDHTExplicitInitial
        , kdcDumpPath = bpKademliaDump
        }

createTransport
    :: (MonadIO m, WithLogger m, Mockable Throw m)
    => String -> Word16 -> m Transport
createTransport ip port = do
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral Const.networkConnectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             })
    transportE <-
        liftIO $ TCP.createTransport "0.0.0.0" (show port) ((,) ip) tcpParams
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
