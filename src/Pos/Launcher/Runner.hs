{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-# OPTIONS -fno-cross-module-specialise #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRealMode
       , runRealBasedMode
       , runServiceMode

       -- * Exported for custom usage in CLI utils
       , setupLoggers
       , bracketDHTInstance
       , runServer
       , runServer_
       , loggerBracket
       , createTransportTCP
       , bracketTransport
       , bracketResources
       , bracketResourcesKademlia
       ) where

import           Control.Concurrent.STM          (newEmptyTMVarIO, newTBQueueIO)
import           Control.Lens                    (each, to, _tail)
import           Control.Monad.Fix               (MonadFix)
import           Data.Default                    (def)
import           Data.Tagged                     (Tagged (..), untag)
import qualified Data.Time                       as Time
import qualified Ether
import           Formatting                      (build, sformat, shown, (%))
import           Mockable                        (CurrentTime, Mockable, MonadMockable,
                                                  Production (..), Throw, bracket,
                                                  killThread, throw)
import           Network.QDisc.Fair              (fairQDisc)
import           Network.Transport.Abstract      (Transport, closeTransport,
                                                  hoistTransport)
import           Network.Transport.Concrete      (concrete)
import qualified Network.Transport.TCP           as TCP
import           Node                            (Node, NodeAction (..), NodeEndPoint,
                                                  ReceiveDelay, Statistics,
                                                  defaultNodeEnvironment,
                                                  hoistSendActions, noReceiveDelay, node,
                                                  simpleNodeEndPoint)
import           Node.Util.Monitor               (setupMonitor, stopMonitor)
import qualified STMContainers.Map               as SM
import qualified System.Metrics                  as Metrics
import qualified System.Metrics.Gauge            as Metrics.Gauge
import           System.Random                   (newStdGen)
import qualified System.Remote.Monitoring        as Monitoring
import qualified System.Remote.Monitoring.Statsd as Monitoring
import           System.Wlog                     (LoggerConfig (..), WithLogger, logError,
                                                  logInfo, logDebug, productionB,
                                                  releaseAllHandlers, setupLogging,
                                                  getLoggerName, usingLoggerName)
import           Universum                       hiding (bracket, finally)

import           Pos.Binary                      ()
import           Pos.Block.BListener             (runBListenerStub)
import           Pos.CLI                         (readLoggerConfig)
import           Pos.Communication               (ActionSpec (..), BiP (..), InSpecs (..),
                                                  MkListeners (..), OutSpecs (..),
                                                  VerInfo (..), allListeners,
                                                  hoistMkListeners)
import           Pos.Communication.PeerState     (PeerStateTag, runPeerStateRedirect)
import qualified Pos.Constants                   as Const
import           Pos.Context                     (BlkSemaphore (..), ConnectedPeers (..),
                                                  NodeContext (..), StartTime (..))
import           Pos.Core                        (Timestamp ())
import           Pos.Crypto                      (createProxySecretKey, encToPublic)
import           Pos.DB                          (MonadDBRead, NodeDBs, runDBPureRedirect)
import           Pos.DB.Block                    (runBlockDBRedirect)
import           Pos.DB.DB                       (initNodeDBs, openNodeDBs,
                                                  runGStateCoreRedirect)
import           Pos.DB.GState                   (getTip)
import           Pos.DB.Misc                     (addProxySecretKey)
import           Pos.Delegation.Class            (DelegationVar)
import           Pos.DHT.Real                    (KademliaDHTInstance,
                                                  KademliaParams (..), startDHTInstance,
                                                  stopDHTInstance)
import           Pos.Discovery                   (DiscoveryContextSum,
                                                  runDiscoveryRedirect)
import           Pos.Genesis                     (genesisLeaders, genesisSeed)
import           Pos.Launcher.Param              (BaseParams (..), LoggingParams (..),
                                                  NodeParams (..))
import           Pos.Lrc.Context                 (LrcContext (..), LrcSyncData (..))
import qualified Pos.Lrc.DB                      as LrcDB
import           Pos.Lrc.Fts                     (followTheSatoshiM)
import           Pos.Security                    (SecurityWorkersClass)
import           Pos.Slotting                    (NtpSlottingVar, SlottingVar,
                                                  mkNtpSlottingVar)
import           Pos.Slotting.MemState.Holder    (runSlotsDataRedirect)
import           Pos.Slotting.Ntp                (runSlotsRedirect)
import           Pos.Ssc.Class                   (SscConstraint, SscNodeContext,
                                                  SscParams, sscCreateNodeContext)
import           Pos.Ssc.Extra                   (SscMemTag, bottomSscState, mkSscState)
import           Pos.Statistics                  (EkgParams (..), StatsdParams (..))
import           Pos.Txp                         (mkTxpLocalData)
import           Pos.Txp.DB                      (genesisFakeTotalStake,
                                                  runBalanceIterBootstrap)
import           Pos.Txp.MemState                (TxpHolderTag, TxpMetrics(..))
#ifdef WITH_EXPLORER
import           Pos.Explorer                    (explorerTxpGlobalSettings)
#else
import           Pos.Txp                         (txpGlobalSettings)
#endif
import           Pos.Update.Context              (UpdateContext (..))
import qualified Pos.Update.DB                   as GState
import           Pos.Update.MemState             (newMemVar)
import           Pos.Util                        (withMaybeFile)
import           Pos.Util.Concurrent.RWVar       as RWV
import           Pos.Util.TimeWarp               (runJsonLogT', runWithoutJsonLogT)
import           Pos.Util.UserSecret             (usKeys)
import           Pos.Worker                      (allWorkersCount)
import           Pos.WorkMode                    (RealMode (..), ServiceMode (..),
                                                  WorkMode)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in 'RealMode'.
runRealMode
    :: forall ssc a.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => DiscoveryContextSum
    -> Transport (RealMode ssc)
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (RealMode ssc) a, OutSpecs)
    -> Production a
runRealMode = runRealBasedMode identity identity
{-# NOINLINE runRealMode #-}

-- | Run activity in something convertible to 'RealMode' and back.
runRealBasedMode
    :: forall ssc m a.
       (SscConstraint ssc, SecurityWorkersClass ssc, WorkMode ssc m)
    => (forall b. m b -> RealMode ssc b)
    -> (forall b. RealMode ssc b -> m b)
    -> DiscoveryContextSum
    -> Transport m
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec m a, OutSpecs)
    -> Production a
runRealBasedMode unwrap wrap discoveryCtx transport np@NodeParams{..} sscnp (ActionSpec action, outSpecs) =
    runRealModeDo
        discoveryCtx
        (hoistTransport unwrap transport)
        np
        sscnp
        listeners
        outSpecs $
    ActionSpec
        $ \vI sendActions -> unwrap . action vI $ hoistSendActions wrap unwrap sendActions
  where
    listeners = hoistMkListeners unwrap wrap allListeners

-- | RealMode runner.
runRealModeDo
    :: forall ssc a.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => DiscoveryContextSum
    -> Transport (RealMode ssc)
    -> NodeParams
    -> SscParams ssc
    -> MkListeners (RealMode ssc)
    -> OutSpecs
    -> ActionSpec (RealMode ssc) a
    -> Production a
runRealModeDo discoveryCtx transport np@NodeParams {..} sscnp listeners outSpecs (ActionSpec action) =
    withMaybeFile npJLFile WriteMode $ \mJLHandle -> runJsonLogT' mJLHandle $ usingLoggerName lpRunnerTag $ do
        initNC <- untag @ssc sscCreateNodeContext sscnp
        modernDBs <- openNodeDBs npRebuildDb npDbPathM
        let allWorkersNum = allWorkersCount @ssc @(RealMode ssc) :: Int
        let runCHHere :: (Mockable CurrentTime m, MonadIO m, MonadMask m)
                      => Ether.ReadersT (NodeContext ssc) m t -> m t
            runCHHere = runCH @ssc allWorkersNum discoveryCtx np initNC modernDBs
        -- TODO [CSL-775] ideally initialization logic should be in scenario.
        runCHHere .
            flip Ether.runReaderT' modernDBs .
            runDBPureRedirect .
            runBlockDBRedirect $
            initNodeDBs @ssc
        initTip <- Ether.runReaderT' (runDBPureRedirect getTip) modernDBs
        stateM <- liftIO SM.newIO
        stateM_ <- liftIO SM.newIO
        slottingVar <-
            Ether.runReaderT'
                (runDBPureRedirect $ mkSlottingVar npSystemStart)
                modernDBs
        txpVar <- mkTxpLocalData mempty initTip
        ntpSlottingVar <- mkNtpSlottingVar
        dlgVar <- RWV.new def

        -- EKG monitoring stuff.
        --
        -- Relevant even if monitoring is turned off (no port given). The
        -- gauge and distribution can be sampled by the server dispatcher
        -- and used to inform a policy for delaying the next receive event.
        --
        -- TODO implement this. Requires time-warp-nt commit
        --   275c16b38a715264b0b12f32c2f22ab478db29e9
        -- in addition to the non-master
        --   fdef06b1ace22e9d91c5a81f7902eb5d4b6eb44f
        -- for flexible EKG setup.
        ekgStore <- liftIO $ Metrics.newStore
        ekgMemPoolSize <- liftIO $ Metrics.createGauge "MemPoolSize" ekgStore
        ekgMemPoolWaitTime <- liftIO $ Metrics.createGauge "MemPoolWaitTime" ekgStore
        ekgMemPoolModifyTime <- liftIO $ Metrics.createGauge "MemPoolModifyTime" ekgStore
        ekgMemPoolQueueLength <- liftIO $ Metrics.createGauge "MemPoolQueueLength" ekgStore

        -- An exponential moving average is used for the time gauges (wait
        -- and modify durations). The parameter alpha is chosen somewhat
        -- arbitrarily.
        -- FIXME take alpha from configuration/CLI, or use a better
        -- estimator.
        let alpha :: Double
            alpha = 0.75
            -- This TxpMetrics specifies what to do when waiting on the
            -- mempool lock, when the mempool lock has been granted, and
            -- when that lock has been released. It updates EKG metrics
            -- and also logs each data point at debug level.
            txpMetrics = TxpMetrics
                { txpMetricsWait = \reason -> do
                      liftIO $ Metrics.Gauge.inc ekgMemPoolQueueLength
                      qlen <- liftIO $ Metrics.Gauge.read ekgMemPoolQueueLength
                      logDebug $ sformat ("MemPool metrics wait: "%shown%" queue length is "%shown) reason qlen

                , txpMetricsAcquire = \timeWaited -> do
                      liftIO $ Metrics.Gauge.dec ekgMemPoolQueueLength
                      timeWaited' <- liftIO $ Metrics.Gauge.read ekgMemPoolWaitTime
                      -- Assume a 0-value estimate means we haven't taken
                      -- any samples yet.
                      let new_ = if timeWaited' == 0
                                then fromIntegral timeWaited
                                else round $ alpha * fromIntegral timeWaited + (1 - alpha) * fromIntegral timeWaited'
                      liftIO $ Metrics.Gauge.set ekgMemPoolWaitTime new_
                      logDebug $ sformat ("MemPool metrics acquire: wait time was "%shown) timeWaited

                , txpMetricsRelease = \timeElapsed memPoolSize -> do
                      liftIO $ Metrics.Gauge.set ekgMemPoolSize (fromIntegral memPoolSize)
                      timeElapsed' <- liftIO $ Metrics.Gauge.read ekgMemPoolModifyTime
                      let new_ = if timeElapsed' == 0
                                then fromIntegral timeElapsed
                                else round $ alpha * fromIntegral timeElapsed + (1 - alpha) * fromIntegral timeElapsed'
                      liftIO $ Metrics.Gauge.set ekgMemPoolModifyTime new_
                      logDebug $ sformat ("MemPool metrics release: modify time was "%shown%" size is "%shown) timeElapsed memPoolSize
                }

        -- TODO [CSL-775] need an effect-free way of running this into IO.
        let runIO :: forall t . RealMode ssc t -> IO t
            runIO (RealMode act) =
               runProduction .
                   runWithoutJsonLogT .
                   usingLoggerName lpRunnerTag .
                   runCHHere .
                   flip Ether.runReadersT
                      ( Tagged @NodeDBs modernDBs
                      , Tagged @SlottingVar slottingVar
                      , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
                      , Tagged @SscMemTag bottomSscState
                      , Tagged @TxpHolderTag (txpVar, txpMetrics)
                      , Tagged @DelegationVar dlgVar
                      , Tagged @PeerStateTag stateM_
                      ) .
                   runDBPureRedirect .
                   runBlockDBRedirect .
                   runSlotsDataRedirect .
                   runSlotsRedirect .
                   runDiscoveryRedirect .
                   runPeerStateRedirect .
                   runGStateCoreRedirect .
                   runBListenerStub $
                   act
        sscState <-
           runCHHere .
           flip Ether.runReadersT
               ( Tagged @NodeDBs modernDBs
               , Tagged @SlottingVar slottingVar
               , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
               ) .
           runSlotsDataRedirect .
           runSlotsRedirect .
           runDBPureRedirect $
           mkSscState @ssc
        runCHHere .
           flip Ether.runReadersT
               ( Tagged @NodeDBs modernDBs
               , Tagged @SlottingVar slottingVar
               , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
               , Tagged @SscMemTag sscState
               , Tagged @TxpHolderTag (txpVar, txpMetrics)
               , Tagged @DelegationVar dlgVar
               , Tagged @PeerStateTag stateM
               ) .
           runDBPureRedirect .
           runBlockDBRedirect .
           runSlotsDataRedirect .
           runSlotsRedirect .
           runDiscoveryRedirect .
           runPeerStateRedirect .
           runGStateCoreRedirect .
           runBListenerStub .
            (\(RealMode m) -> m) .
            runServer (simpleNodeEndPoint transport) (const noReceiveDelay) listeners outSpecs (startMonitoring ekgStore runIO) stopMonitoring . ActionSpec $
               \vI sa -> nodeStartMsg npBaseParams >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams npBaseParams

    startMonitoring ekgStore (runIO :: forall t . RealMode ssc t -> IO t) node' =
        case npEnableMetrics of
            False -> return Nothing
            True -> Just <$> do
                ekgStore' <- setupMonitor runIO node' ekgStore
                liftIO $ Metrics.registerGcMetrics ekgStore'
                mEkgServer <- case npEkgParams of
                    Nothing -> return Nothing
                    Just (EkgParams {..}) -> Just <$> do
                        liftIO $ Monitoring.forkServerWith ekgStore' ekgHost ekgPort
                mStatsdServer <- case npStatsdParams of
                    Nothing -> return Nothing
                    Just (StatsdParams {..}) -> Just <$> do
                        let statsdOptions = Monitoring.defaultStatsdOptions
                                { Monitoring.host = statsdHost
                                , Monitoring.port = statsdPort
                                , Monitoring.flushInterval = statsdInterval
                                , Monitoring.debug = statsdDebug
                                , Monitoring.prefix = statsdPrefix
                                , Monitoring.suffix = statsdSuffix
                                }
                        liftIO $ Monitoring.forkStatsd statsdOptions ekgStore'
                return (mEkgServer, mStatsdServer)

    stopMonitoring Nothing = return ()
    stopMonitoring (Just (mEkg, mStatsd)) = do
        whenJust mStatsd (killThread . Monitoring.statsdThreadId)
        whenJust mEkg stopMonitor

-- | Create new 'SlottingVar' using data from DB.
mkSlottingVar :: (MonadIO m, MonadDBRead m) => Timestamp -> m SlottingVar
mkSlottingVar sysStart = do
    sd <- GState.getSlottingData
    (sysStart, ) <$> newTVarIO sd

-- | ServiceMode runner.
runServiceMode
    :: Transport ServiceMode
    -> BaseParams
    -> MkListeners ServiceMode
    -> OutSpecs
    -> ActionSpec ServiceMode a
    -> Production a
runServiceMode transport bp@BaseParams {..} listeners outSpecs (ActionSpec action) = do
    stateM <- liftIO SM.newIO
    runWithoutJsonLogT $ usingLoggerName (lpRunnerTag bpLoggingParams) .
        flip (Ether.runReaderT @PeerStateTag) stateM .
        runPeerStateRedirect .
        (\(ServiceMode m) -> m) .
        runServer_ transport listeners outSpecs . ActionSpec $ \vI sa ->
        nodeStartMsg bp >> action vI sa
{-# NOINLINE runServiceMode #-}

runServer
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => (m (Statistics m) -> NodeEndPoint m)
    -> (m (Statistics m) -> ReceiveDelay m)
    -> MkListeners m
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer mkTransport mkReceiveDelay mkL (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo: "%build) ourVerInfo
    node mkTransport mkReceiveDelay stdGen BiP ourVerInfo defaultNodeEnvironment $ \__node ->
        NodeAction mkListeners' $ \sendActions ->
            bracket (withNode __node) afterNode (const (action ourVerInfo sendActions))
  where
    InSpecs ins = inSpecs mkL
    OutSpecs outs = outSpecs mkL
    ourVerInfo =
        VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
    mkListeners' theirVerInfo = do
        --logDebug $ sformat ("Incoming connection: theirVerInfo="%build) theirVerInfo
        mkListeners mkL ourVerInfo theirVerInfo
runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m -> MkListeners m -> OutSpecs -> ActionSpec m b -> m b
runServer_ transport mkl outSpecs =
    runServer (simpleNodeEndPoint transport) (const noReceiveDelay) mkl outSpecs acquire release
  where
    acquire = const pass
    release = const pass

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH
    :: forall ssc m a.
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , MonadIO m
       , MonadCatch m
       , Mockable CurrentTime m)
    => Int
    -> DiscoveryContextSum
    -> NodeParams
    -> SscNodeContext ssc
    -> NodeDBs
    -> Ether.ReadersT (NodeContext ssc) m a
    -> m a
runCH allWorkersNum discoveryCtx params@NodeParams {..} sscNodeContext db act = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncBlkSemaphore <- BlkSemaphore <$> newEmptyMVar
    ucUpdateSemaphore <- newEmptyMVar

    -- TODO [CSL-775] lrc initialization logic is duplicated.
    epochDef <- Ether.runReaderT' (runDBPureRedirect LrcDB.getEpochDefault) db
    lcLrcSync <- newTVarIO (LrcSyncData True epochDef)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey npSecretKey) eternity . encToPublic
        ownPSKs = npUserSecret ^.. usKeys._tail.each.to makeOwnPSK
    Ether.runReaderT' (runDBPureRedirect $ for_ ownPSKs addProxySecretKey) db

    ncUserSecret <- newTVarIO $ npUserSecret
    ncBlockRetrievalQueue <- liftIO $
        newTBQueueIO Const.blockRetrievalQueueSize
    ncInvPropagationQueue <- liftIO $
        newTBQueueIO Const.propagationQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    ncProgressHeader <- liftIO newEmptyTMVarIO
    ncShutdownFlag <- newTVarIO False
    ncShutdownNotifyQueue <- liftIO $ newTBQueueIO allWorkersNum
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    ncGenesisLeaders <- if Const.isDevelopment
                        then pure $ genesisLeaders npCustomUtxo
                        else runBalanceIterBootstrap $
                             followTheSatoshiM genesisSeed genesisFakeTotalStake
    ucMemState <- newMemVar
    ucDownloadingUpdates <- newTVarIO mempty
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    let ctx =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
            , ncSscContext = sscNodeContext
            , ncLrcContext = LrcContext {..}
            , ncDiscoveryContext = discoveryCtx
            , ncUpdateContext = UpdateContext {..}
            , ncNodeParams = params
            , ncSendLock = Nothing
#ifdef WITH_EXPLORER
            , ncTxpGlobalSettings = explorerTxpGlobalSettings
#else
            , ncTxpGlobalSettings = txpGlobalSettings
#endif
            , .. }
    Ether.runReadersT act ctx

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: WithLogger m => BaseParams -> m ()
nodeStartMsg BaseParams {..} = do
    logInfo msg1
  where
    msg1 = sformat ("Application: " %build% ", last known block version " %build)
                   Const.curSoftwareVersion Const.lastKnownBlockVersion

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    -- TODO: introduce Maybe FilePath builder for filePrefix
    let cfgBuilder = productionB <>
                     (mempty { _lcFilePrefix = lpHandlerPrefix })
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers params = setupLogging =<< getRealLoggerConfig params

-- | RAII for node starter.
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) releaseAllHandlers

bracketDHTInstance
    :: BaseParams
    -> KademliaParams
    -> (KademliaDHTInstance -> Production a)
    -> Production a
bracketDHTInstance BaseParams {..} kp action = bracket acquire release action
  where
    --withLog = usingLoggerName $ lpRunnerTag bpLoggingParams
    acquire = usingLoggerName (lpRunnerTag bpLoggingParams) (startDHTInstance instConfig)
    release = usingLoggerName (lpRunnerTag bpLoggingParams) . stopDHTInstance
    instConfig =
        kp
        { kpPeers = ordNub $ kpPeers kp ++ Const.defaultPeers
        }

createTransportTCP
    :: (MonadIO m, WithLogger m, Mockable Throw m)
    => TCP.TCPAddr
    -> m (Transport m)
createTransportTCP addrInfo = do
    loggerName <- getLoggerName
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral Const.networkConnectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
             , TCP.tcpServerExceptionHandler = \e ->
                     usingLoggerName (loggerName <> "transport") $
                         logError $ sformat ("Exception in tcp server: " % shown) e
             })
    transportE <-
        liftIO $ TCP.createTransport addrInfo tcpParams
    case transportE of
        Left e -> do
            logError $ sformat ("Error creating TCP transport: " % shown) e
            throw e
        Right transport -> return (concrete transport)

bracketTransport
    :: TCP.TCPAddr
    -> (Transport Production -> Production a)
    -> Production a
bracketTransport tcpAddr =
    bracket (createTransportTCP tcpAddr) (closeTransport)

-- | Bracket a transport for use with a static set of peers (for discovery).
bracketResources
    :: BaseParams
    -> TCP.TCPAddr
    -> (Transport Production -> Production a)
    -> IO a
bracketResources bp tcpAddr action =
    loggerBracket (bpLoggingParams bp) .
    runProduction $
        -- Both the DHT and Transport are problematic here:
        -- 1. We assume you'll want a DHT.
        -- 2. We assume your transport takes an IP/port.
        bracketTransport tcpAddr action

-- | Bracket a transport and a Kademlia node, using the latter for discovery.
bracketResourcesKademlia
    :: BaseParams
    -> TCP.TCPAddr
    -> KademliaParams
    -> (KademliaDHTInstance -> Transport Production -> Production a)
    -> IO a
bracketResourcesKademlia bp tcpAddr kp action =
    loggerBracket (bpLoggingParams bp) .
    runProduction .
    bracketDHTInstance bp kp $ \kademliaInstance ->
        bracketTransport tcpAddr $ action kademliaInstance
