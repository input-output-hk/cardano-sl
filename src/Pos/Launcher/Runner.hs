{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRawRealMode
       , runProductionMode
       , runStatsMode
       , runServiceMode
       , runStaticMode

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

import           Control.Concurrent.STM      (newEmptyTMVarIO, newTBQueueIO)
import           Control.Lens                (each, to, _tail)
import           Control.Monad.Fix           (MonadFix)
import           Data.Default                (def)
import           Data.Tagged                 (untag)
import           Data.Text                   (pack)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (CurrentTime, Mockable, MonadMockable, 
                                              Bracket, Production (..), Throw, 
                                              bracket, finally, throw)
import           Network.QDisc.Fair          (fairQDisc)
import           Network.Transport.Abstract  (Transport, closeTransport, hoistTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node                        (Node, NodeAction (..),
                                              defaultNodeEnvironment, hoistSendActions,
                                              node, simpleNodeEndPoint,
                                              NodeEndPoint, Statistics,
                                              ReceiveDelay, noReceiveDelay)
import           Node.Util.Monitor           (setupMonitor, stopMonitor)
import qualified STMContainers.Map           as SM
import           System.IO                   (hClose, hSetBuffering,
                                              BufferMode (NoBuffering))
import           System.Random               (newStdGen)
import qualified System.Remote.Monitoring    as Monitoring
import qualified System.Metrics              as Metrics
import qualified System.Metrics.Gauge        as Metrics.Gauge
import           System.Wlog                 (LoggerConfig (..), WithLogger, logError,
                                              logInfo, logDebug, productionB,
                                              releaseAllHandlers, setupLogging,
                                              usingLoggerName)
import           Universum                   hiding (bracket, finally)

import           Pos.Binary                  ()
import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication           (ActionSpec (..), BiP (..), InSpecs (..),
                                              ListenersWithOut, NodeId, OutSpecs (..),
                                              PeerId (..), VerInfo (..), allListeners,
                                              hoistListenerSpec, unpackLSpecs)
import           Pos.Communication.PeerState (runPeerStateHolder)
import qualified Pos.Constants               as Const
import           Pos.Context                 (ContextHolder, NodeContext (..),
                                              runContextHolder)
import           Pos.Core                    (Timestamp ())
import           Pos.Crypto                  (createProxySecretKey, encToPublic)
import           Pos.DB                      (DBHolder, MonadDB, NodeDBs, runDBHolder)
import           Pos.DB.DB                   (initNodeDBs, openNodeDBs)
import           Pos.DB.GState               (getTip)
import           Pos.DB.Misc                 (addProxySecretKey)
import           Pos.Delegation.Holder       (runDelegationT)
import           Pos.DHT.Real                (KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              KademliaParams (..), startDHTInstance,
                                              stopDHTInstance)
import           Pos.Discovery.Holders       (DiscoveryKademliaT, DiscoveryConstT,
                                              runDiscoveryConstT,
                                              runDiscoveryKademliaT)
import           Pos.Genesis                 (genesisLeaders, genesisSeed)
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NodeParams (..))
import           Pos.Lrc.Context             (LrcContext (..), LrcSyncData (..))
import qualified Pos.Lrc.DB                  as LrcDB
import           Pos.Lrc.Fts                 (followTheSatoshiM)
import           Pos.Slotting                (SlottingVar, mkNtpSlottingVar,
                                              runNtpSlotting, runSlottingHolder)
import           Pos.Ssc.Class               (SscConstraint, SscNodeContext, SscParams,
                                              sscCreateNodeContext)
import           Pos.Ssc.Extra               (ignoreSscHolder, mkStateAndRunSscHolder)
import           Pos.Statistics              (StatsT, NoStatsT, getNoStatsT, runStatsT')
import           Pos.Txp                     (mkTxpLocalData, runTxpHolder,
                                              TxpMetrics (..))
import           Pos.Txp.DB                  (genesisFakeTotalStake,
                                              runBalanceIterBootstrap)
#ifdef WITH_EXPLORER
import           Pos.Explorer                (explorerTxpGlobalSettings)
#else
import           Pos.Txp                     (txpGlobalSettings)
#endif
import           Pos.Update.Context          (UpdateContext (..))
import qualified Pos.Update.DB               as GState
import           Pos.Update.MemState         (newMemVar)
import           Pos.Util.UserSecret         (usKeys)
import           Pos.Worker                  (allWorkersCount)
import           Pos.WorkMode                (ProductionMode, RawRealMode, ServiceMode,
                                              StaticMode, StatsMode)

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | RawRealMode runner.
runRawRealMode
    :: forall ssc a.
       SscConstraint ssc
    => PeerId
    -> Transport (RawRealMode ssc)
    -> NodeParams
    -> SscParams ssc
    -> RawRealMode ssc (ListenersWithOut (RawRealMode ssc))
    -> OutSpecs
    -> ActionSpec (RawRealMode ssc) a
    -> Production a
runRawRealMode peerId transport np@NodeParams {..} sscnp listeners outSpecs (ActionSpec action) =
    usingLoggerName lpRunnerTag $ do
        withMaybeFile npJLFile WriteMode $ \mh -> do
            whenJust mh $ \h -> liftIO $ hSetBuffering h NoBuffering
            jlVar     <- maybe (pure Nothing) (fmap Just . newMVar) mh
            initNC    <- untag @ssc sscCreateNodeContext sscnp
            modernDBs <- openNodeDBs npRebuildDb npDbPathM
            let allWorkersNum = allWorkersCount @ssc @(ProductionMode ssc) :: Int
            -- TODO [CSL-775] ideally initialization logic should be in scenario.
            runCH @ssc jlVar allWorkersNum np initNC modernDBs $ initNodeDBs
            initTip <- runDBHolder modernDBs getTip
            stateM <- liftIO SM.newIO
            stateM_ <- liftIO SM.newIO
            slottingVar <- runDBHolder  modernDBs $ mkSlottingVar npSystemStart
            txpVar <- mkTxpLocalData mempty initTip
            ntpSlottingVar <- mkNtpSlottingVar

            -- EKG monitoring stuff.
            --
            -- Relevant even if monitoring is turned off (no port given). The
            -- gauge and distribution can be sampled by the server dispatcher
            -- and used to inform a policy for delaying the next receive event.
            --
            ekgStore <- liftIO $ Metrics.newStore
            ekgMemPoolSize <- liftIO $ Metrics.createGauge (pack "MemPoolSize") ekgStore
            ekgMemPoolWaitTime <- liftIO $ Metrics.createGauge (pack "MemPoolWaitTime") ekgStore
            ekgMemPoolModifyTime <- liftIO $ Metrics.createGauge (pack "MemPoolModifyTime") ekgStore
            ekgMemPoolQueueLength <- liftIO $ Metrics.createGauge (pack "MemPoolQueueLength") ekgStore


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
                          let new = if timeWaited' == 0
                                    then fromIntegral timeWaited
                                    else round $ alpha * fromIntegral timeWaited + (1 - alpha) * fromIntegral timeWaited'
                          liftIO $ Metrics.Gauge.set ekgMemPoolWaitTime new
                          logDebug $ sformat ("MemPool metrics acquire: wait time was "%shown) timeWaited

                    , txpMetricsRelease = \timeElapsed memPoolSize -> do
                          liftIO $ Metrics.Gauge.set ekgMemPoolSize (fromIntegral memPoolSize)
                          timeElapsed' <- liftIO $ Metrics.Gauge.read ekgMemPoolModifyTime
                          let new = if timeElapsed' == 0
                                    then fromIntegral timeElapsed
                                    else round $ alpha * fromIntegral timeElapsed + (1 - alpha) * fromIntegral timeElapsed'
                          liftIO $ Metrics.Gauge.set ekgMemPoolModifyTime new
                          logDebug $ sformat ("MemPool metrics release: modify time was "%shown%" size is "%shown) timeElapsed memPoolSize
                    }

            -- TODO [CSL-775] need an effect-free way of running this into IO.
            let runIO :: forall t . RawRealMode ssc t -> IO t
                runIO = runProduction .
                        usingLoggerName lpRunnerTag .
                        runCH @ssc jlVar allWorkersNum np initNC modernDBs .
                        runSlottingHolder slottingVar .
                        runNtpSlotting (npUseNTP, ntpSlottingVar) .
                        ignoreSscHolder .
                        runTxpHolder txpVar txpMetrics .
                        runDelegationT def .
                        runPeerStateHolder stateM_

            -- To start monitoring, add the time-warp metrics and the GC
            -- metrics then spin up the server.
            let startMonitoring node' = case lpEkgPort of
                    Nothing   -> return Nothing
                    Just port -> Just <$> do
                         ekgStore' <- setupMonitor runIO node' ekgStore
                         liftIO $ Metrics.registerGcMetrics ekgStore'
                         liftIO $ Monitoring.forkServerWith ekgStore' "127.0.0.1" port

            let stopMonitoring it = whenJust it stopMonitor

            let mkTransport = simpleNodeEndPoint transport

            -- TODO have the receive delay for the dispatcher thread grow as
            -- the mempool queue length grows.
            -- The mempool queue will clear without any need to draw more data
            -- from the network, so it should be ok to do this.
            -- It might look something like this:
            {-
            let mkReceiveDelay _ = do
                    qlength <- liftIO $ Metrics.Gauge.read ekgMemPoolQueueLength
                    waitTime <- liftIO $ Metrics.Gauge.read ekgMemPoolWaitTime
                    let expectedWait = qlength * waitTime
                    if expectedWait < 100
                    then return Nothing
                    else do
                        let actualDelay = fromIntegral expectedWait
                        logDebug $ sformat ("delaying network input for "%shown) actualDelay
                        return $ Just actualDelay
            -}
            let mkReceiveDelay _ = pure Nothing

            runCH jlVar allWorkersNum np initNC modernDBs .
                runSlottingHolder slottingVar .
                runNtpSlotting (npUseNTP, ntpSlottingVar) .
                (mkStateAndRunSscHolder @ssc) .
                runTxpHolder txpVar txpMetrics .
                runDelegationT def .
                runPeerStateHolder stateM .
                runServer peerId mkTransport mkReceiveDelay listeners outSpecs startMonitoring stopMonitoring . ActionSpec $
                    \vI sa -> nodeStartMsg npBaseParams >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams npBaseParams

-- | Create new 'SlottingVar' using data from DB.
mkSlottingVar :: MonadDB m => Timestamp -> m SlottingVar
mkSlottingVar sysStart = do
    sd <- GState.getSlottingData
    (sysStart, ) <$> newTVarIO sd

-- | ServiceMode runner.
runServiceMode
    :: PeerId
    -> Transport ServiceMode
    -> BaseParams
    -> ListenersWithOut ServiceMode
    -> OutSpecs
    -> ActionSpec ServiceMode a
    -> Production a
runServiceMode peerId transport bp@BaseParams {..} listeners outSpecs (ActionSpec action) = do
    stateM <- liftIO SM.newIO
    usingLoggerName (lpRunnerTag bpLoggingParams) .
        runPeerStateHolder stateM .
        runServer_ peerId transport listeners outSpecs . ActionSpec $ \vI sa ->
        nodeStartMsg bp >> action vI sa

runServer
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => PeerId
    -> (m (Statistics m) -> NodeEndPoint m)
    -> (m (Statistics m) -> ReceiveDelay m)
    -> m (ListenersWithOut m)
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer peerId mkTransport mkReceiveDelay packedLS_M (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    packedLS  <- packedLS_M
    let (listeners', InSpecs ins, OutSpecs outs) = unpackLSpecs packedLS
        ourVerInfo =
            VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
        listeners = listeners' ourVerInfo
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo "%build) ourVerInfo
    node mkTransport mkReceiveDelay stdGen BiP (peerId, ourVerInfo) defaultNodeEnvironment $ \__node ->
        NodeAction listeners $ \sendActions -> do
            t <- withNode __node
            action ourVerInfo sendActions `finally` afterNode t

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => PeerId -> Transport m -> ListenersWithOut m -> OutSpecs -> ActionSpec m b -> m b
runServer_ peerId transport packedLS outSpecs =
    runServer peerId (simpleNodeEndPoint transport) (const noReceiveDelay) (pure packedLS) outSpecs acquire release
  where
    acquire = const pass
    release = const pass

-- | ProductionMode runner.
runProductionMode
    :: forall ssc a.
       (SscConstraint ssc)
    => PeerId
    -> Transport (ProductionMode ssc)
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (ProductionMode ssc) a, OutSpecs)
    -> Production a
runProductionMode peerId transport kinst np@NodeParams {..} sscnp (ActionSpec action, outSpecs) =
    runRawRealMode
        peerId
        (hoistTransport hoistDown transport)
        np
        sscnp
        listeners
        outSpecs $
    ActionSpec $ \vI sendActions ->
        hoistDown . action vI $ hoistSendActions hoistUp hoistDown sendActions
  where
    hoistUp = lift . lift
    hoistDown :: forall t . NoStatsT (DiscoveryKademliaT (RawRealMode ssc)) t -> RawRealMode ssc t
    hoistDown = runDiscoveryKademliaT kinst . getNoStatsT
    listeners =
        hoistDown $
        first (hoistListenerSpec hoistDown hoistUp <$>) <$>
        allListeners

-- | StatsMode runner.
-- [CSL-169]: spawn here additional listener, which would accept stat queries
-- can be done as part of refactoring (or someone who will refactor will create new issue).
runStatsMode
    :: forall ssc a.
       (SscConstraint ssc)
    => PeerId
    -> Transport (StatsMode ssc)
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (StatsMode ssc) a, OutSpecs)
    -> Production a
runStatsMode peerId transport kinst np@NodeParams{..} sscnp (ActionSpec action, outSpecs) = do
    statMap <- liftIO SM.newIO
    let hoistUp = lift . lift
    let hoistDown :: forall t . StatsT (DiscoveryKademliaT (RawRealMode ssc)) t -> RawRealMode ssc t
        hoistDown = runDiscoveryKademliaT kinst . runStatsT' statMap
    let listeners =
            hoistDown $
            first (hoistListenerSpec hoistDown hoistUp <$>) <$>
            allListeners
    runRawRealMode
        peerId
        (hoistTransport hoistDown transport)
        np
        sscnp
        listeners
        outSpecs .
        ActionSpec $ \vI sendActions ->
            hoistDown . action vI $ hoistSendActions hoistUp hoistDown sendActions

runStaticMode
    :: forall ssc a.
       (SscConstraint ssc)
    => PeerId
    -> Transport (StaticMode ssc)
    -> Set NodeId
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (StaticMode ssc) a, OutSpecs)
    -> Production a
runStaticMode peerId transport peers np@NodeParams {..} sscnp (ActionSpec action, outSpecs) =
    runRawRealMode
        peerId
        (hoistTransport hoistDown transport)
        np
        sscnp
        listeners
        outSpecs $
    ActionSpec $ \vI sendActions ->
        hoistDown . action vI $ hoistSendActions hoistUp hoistDown sendActions
  where
    hoistUp = lift . lift
    hoistDown :: forall m t . NoStatsT (DiscoveryConstT m) t -> m t
    hoistDown = runDiscoveryConstT peers . getNoStatsT
    listeners =
        hoistDown $
        first (hoistListenerSpec hoistDown hoistUp <$>) <$>
        allListeners

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH :: forall ssc m a . 
         ( SscConstraint ssc
         , MonadIO m
         , MonadMask m
         , Mockable CurrentTime m
         , Mockable Bracket m
         )
      => Maybe (MVar Handle)
      -> Int 
      -> NodeParams 
      -> SscNodeContext ssc 
      -> NodeDBs 
      -> DBHolder (ContextHolder ssc m) a 
      -> m a
runCH mJLHandle allWorkersNum params@NodeParams {..} sscNodeContext db act = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncBlkSemaphore <- newEmptyMVar
    ucUpdateSemaphore <- newEmptyMVar

    -- TODO [CSL-775] lrc initialization logic is duplicated.
    epochDef <- runDBHolder db LrcDB.getEpochDefault
    lcLrcSync <- newTVarIO (LrcSyncData True epochDef)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey npSecretKey) eternity . encToPublic
        ownPSKs = npUserSecret ^.. usKeys._tail.each.to makeOwnPSK
    runDBHolder db $ for_ ownPSKs addProxySecretKey

    ncUserSecret <- newTVarIO $ npUserSecret
    ncBlockRetrievalQueue <- liftIO $
        newTBQueueIO Const.blockRetrievalQueueSize
    ncInvPropagationQueue <- liftIO $
        newTBQueueIO Const.propagationQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    ncProgressHeader <- liftIO newEmptyTMVarIO
    ncShutdownFlag <- newTVarIO False
    ncShutdownNotifyQueue <- liftIO $ newTBQueueIO allWorkersNum
    ncStartTime <- liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    ncGenesisLeaders <- if Const.isDevelopment
                        then pure $ genesisLeaders npCustomUtxo
                        else runBalanceIterBootstrap $
                             followTheSatoshiM genesisSeed genesisFakeTotalStake
    ucMemState <- newMemVar
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    let ncJLFile = mJLHandle 
        ctx      =
            NodeContext
            { ncConnectedPeers = peersVar
            , ncSscContext = sscNodeContext
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
    runContextHolder ctx (runDBHolder db act)

withMaybeFile :: (Mockable Bracket m, MonadIO m) => Maybe FilePath -> IOMode -> (Maybe Handle -> m a) -> m a
withMaybeFile Nothing _ m        = m Nothing
withMaybeFile (Just path) mode m = bracket 
    (openFile path mode)
    (liftIO . hClose)
    (m . Just)

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------

nodeStartMsg :: WithLogger m => BaseParams -> m ()
nodeStartMsg BaseParams {..} = logInfo msg
  where
    msg = sformat ("Started node.")

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
bracketDHTInstance BaseParams {..} KademliaParams {..} action = bracket acquire release action
  where
    --withLog = usingLoggerName $ lpRunnerTag bpLoggingParams
    acquire = usingLoggerName (lpRunnerTag bpLoggingParams) (startDHTInstance instConfig)
    release = usingLoggerName (lpRunnerTag bpLoggingParams) . stopDHTInstance
    instConfig =
        KademliaDHTInstanceConfig
        { kdcKey = kpKey
        , kdcHost = fst kpNetworkAddress
        , kdcPort = snd kpNetworkAddress
        , kdcInitialPeers = ordNub $ kpPeers ++ Const.defaultPeers
        , kdcExplicitInitial = kpExplicitInitial
        , kdcDumpPath = kpDump
        }

createTransportTCP
    :: (MonadIO m, WithLogger m, Mockable Throw m)
    => TCP.TCPAddr
    -> m (Transport m)
createTransportTCP addrInfo = do
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral Const.networkConnectionTimeout
             , TCP.tcpNewQDisc = fairQDisc $ \_ -> return Nothing
             -- Will check the peer's claimed host against the observed host
             -- when new connections are made. This prevents an easy denial
             -- of service attack.
             , TCP.tcpCheckPeerHost = True
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
