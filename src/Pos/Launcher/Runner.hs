{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRawRealMode
       , runRawKBasedMode
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

import           Control.Concurrent.STM       (newEmptyTMVarIO, newTBQueueIO)
import           Control.Lens                 (each, to, _tail)
import           Control.Monad.Fix            (MonadFix)
import           Data.Default                 (def)
import           Data.Tagged                  (Tagged (..), untag)
import qualified Data.Time                    as Time
import qualified Ether
import           Formatting                   (build, sformat, shown, (%))
import           Mockable                     (CurrentTime, Mockable, MonadMockable,
                                               Production (..), Throw, bracket, finally,
                                               throw)
import           Network.QDisc.Fair           (fairQDisc)
import           Network.Transport.Abstract   (Transport, closeTransport, hoistTransport)
import           Network.Transport.Concrete   (concrete)
import qualified Network.Transport.TCP        as TCP
import           Node                         (Node, NodeAction (..),
                                               defaultNodeEnvironment, hoistSendActions,
                                               node, simpleNodeEndPoint)
import           Node.Util.Monitor            (setupMonitor, stopMonitor)
import qualified STMContainers.Map            as SM
import           System.Random                (newStdGen)
import           System.Wlog                  (LoggerConfig (..), WithLogger, logError,
                                               logInfo, productionB, releaseAllHandlers,
                                               setupLogging, usingLoggerName)
import           Universum                    hiding (bracket, finally)

import           Pos.Binary                   ()
import           Pos.Block.BListener          (runBListenerStub)
import           Pos.CLI                      (readLoggerConfig)
import           Pos.Client.Txp.Balances      (runBalancesRedirect)
import           Pos.Client.Txp.History       (runTxHistoryRedirect)
import           Pos.Communication            (ActionSpec (..), BiP (..), InSpecs (..),
                                               ListenersWithOut, NodeId, OutSpecs (..),
                                               PeerId (..), VerInfo (..), allListeners,
                                               hoistListenerSpec, unpackLSpecs)
import           Pos.Communication.PeerState  (PeerStateTag, runPeerStateRedirect)
import qualified Pos.Constants                as Const
import           Pos.Context                  (BlkSemaphore (..), ConnectedPeers (..),
                                               NodeContext (..), StartTime (..))
import           Pos.Core                     (Timestamp ())
import           Pos.Crypto                   (createProxySecretKey, encToPublic)
import           Pos.DB                       (MonadDB, NodeDBs)
import           Pos.DB.DB                    (initNodeDBs, openNodeDBs,
                                               runGStateCoreRedirect)
import           Pos.DB.GState                (getTip)
import           Pos.DB.Misc                  (addProxySecretKey)
import           Pos.Delegation.Class         (DelegationWrap)
import           Pos.DHT.Real                 (KademliaDHTInstance,
                                               KademliaDHTInstanceConfig (..),
                                               KademliaParams (..), startDHTInstance,
                                               stopDHTInstance)
import           Pos.Discovery.Holders        (runDiscoveryConstT, runDiscoveryKademliaT)
import           Pos.Genesis                  (genesisLeaders, genesisSeed)
import           Pos.Launcher.Param           (BaseParams (..), LoggingParams (..),
                                               NodeParams (..))
import           Pos.Lrc.Context              (LrcContext (..), LrcSyncData (..))
import qualified Pos.Lrc.DB                   as LrcDB
import           Pos.Lrc.Fts                  (followTheSatoshiM)
import           Pos.Slotting                 (NtpSlottingVar, SlottingVar,
                                               mkNtpSlottingVar)
import           Pos.Slotting.MemState.Holder (runSlotsDataRedirect)
import           Pos.Slotting.Ntp             (runSlotsRedirect)
import           Pos.Ssc.Class                (SscConstraint, SscNodeContext, SscParams,
                                               sscCreateNodeContext)
import           Pos.Ssc.Extra                (SscMemTag, bottomSscState, mkSscState)
import           Pos.Statistics               (getNoStatsT, runStatsT')
import           Pos.Txp                      (mkTxpLocalData)
import           Pos.Txp.DB                   (genesisFakeTotalStake,
                                               runBalanceIterBootstrap)
import           Pos.Txp.MemState             (TxpHolderTag)
import           Pos.Wallet.WalletMode        (runBlockchainInfoRedirect,
                                               runUpdatesRedirect)
#ifdef WITH_EXPLORER
import           Pos.Explorer                 (explorerTxpGlobalSettings)
#else
import           Pos.Txp                      (txpGlobalSettings)
#endif
import           Pos.Update.Context           (UpdateContext (..))
import qualified Pos.Update.DB                as GState
import           Pos.Update.MemState          (newMemVar)
import           Pos.Util.JsonLog             (JLFile (..))
import           Pos.Util.UserSecret          (usKeys)
import           Pos.Worker                   (allWorkersCount)
import           Pos.WorkMode                 (ProductionMode, RawRealMode, RawRealModeK,
                                               ServiceMode, StaticMode, StatsMode,
                                               WorkMode)

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
        initNC <- untag @ssc sscCreateNodeContext sscnp
        modernDBs <- openNodeDBs npRebuildDb npDbPathM
        let allWorkersNum = allWorkersCount @ssc @(ProductionMode ssc) :: Int
        -- TODO [CSL-775] ideally initialization logic should be in scenario.
        runCH @ssc allWorkersNum np initNC modernDBs $
            flip Ether.runReaderT' modernDBs $ initNodeDBs @ssc
        initTip <- Ether.runReaderT' getTip modernDBs
        stateM <- liftIO SM.newIO
        stateM_ <- liftIO SM.newIO
        slottingVar <- Ether.runReaderT' (mkSlottingVar npSystemStart) modernDBs
        txpVar <- mkTxpLocalData mempty initTip
        ntpSlottingVar <- mkNtpSlottingVar

        -- TODO [CSL-775] need an effect-free way of running this into IO.
        let runIO :: forall t . RawRealMode ssc t -> IO t
            runIO act = do
               deleg <- newTVarIO def
               runProduction .
                   usingLoggerName lpRunnerTag .
                   runCH @ssc allWorkersNum np initNC modernDBs .
                   flip Ether.runReadersT
                      ( Tagged @NodeDBs modernDBs
                      , Tagged @SlottingVar slottingVar
                      , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
                      , Tagged @SscMemTag bottomSscState
                      , Tagged @TxpHolderTag txpVar
                      , Tagged @(TVar DelegationWrap) deleg
                      , Tagged @PeerStateTag stateM_
                      ) .
                   runSlotsDataRedirect .
                   runSlotsRedirect .
                   runBalancesRedirect .
                   runTxHistoryRedirect .
                   runPeerStateRedirect .
                   runGStateCoreRedirect .
                   runUpdatesRedirect .
                   runBlockchainInfoRedirect .
                   runBListenerStub $
                   act

        let startMonitoring node' = case lpEkgPort of
                Nothing   -> return Nothing
                Just port -> Just <$> setupMonitor port runIO node'

        let stopMonitoring it = whenJust it stopMonitor

        sscState <-
           runCH @ssc allWorkersNum np initNC modernDBs .
           flip Ether.runReadersT
               ( Tagged @NodeDBs modernDBs
               , Tagged @SlottingVar slottingVar
               , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
               ) .
           runSlotsDataRedirect .
           runSlotsRedirect $
           mkSscState @ssc
        deleg <- newTVarIO def
        runCH allWorkersNum np initNC modernDBs .
           flip Ether.runReadersT
               ( Tagged @NodeDBs modernDBs
               , Tagged @SlottingVar slottingVar
               , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
               , Tagged @SscMemTag sscState
               , Tagged @TxpHolderTag txpVar
               , Tagged @(TVar DelegationWrap) deleg
               , Tagged @PeerStateTag stateM
               ) .
           runSlotsDataRedirect .
           runSlotsRedirect .
           runBalancesRedirect .
           runTxHistoryRedirect .
           runPeerStateRedirect .
           runGStateCoreRedirect .
           runUpdatesRedirect .
           runBlockchainInfoRedirect .
           runBListenerStub .
           runServer peerId transport listeners outSpecs startMonitoring stopMonitoring . ActionSpec $
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
        flip (Ether.runReaderT @PeerStateTag) stateM .
        runPeerStateRedirect .
        runServer_ peerId transport listeners outSpecs . ActionSpec $ \vI sa ->
        nodeStartMsg bp >> action vI sa

runServer
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => PeerId
    -> Transport m
    -> m (ListenersWithOut m)
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer peerId transport packedLS_M (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    packedLS  <- packedLS_M
    let (listeners', InSpecs ins, OutSpecs outs) = unpackLSpecs packedLS
        ourVerInfo =
            VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
        listeners = listeners' ourVerInfo
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo "%build) ourVerInfo
    node (simpleNodeEndPoint transport) stdGen BiP (peerId, ourVerInfo) defaultNodeEnvironment $ \__node ->
        NodeAction listeners $ \sendActions -> do
            t <- withNode __node
            action ourVerInfo sendActions `finally` afterNode t

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => PeerId -> Transport m -> ListenersWithOut m -> OutSpecs -> ActionSpec m b -> m b
runServer_ peerId transport packedLS outSpecs =
    runServer peerId transport (pure packedLS) outSpecs acquire release
  where
    acquire = const pass
    release = const pass

-- | Launch some mode, providing way to convert it to 'RawRealMode' and back.
runRawKBasedMode
    :: forall ssc m a.
       (SscConstraint ssc, WorkMode ssc m)
    => (forall b. m b -> RawRealModeK ssc b)
    -> (forall b. RawRealModeK ssc b -> m b)
    -> PeerId
    -> Transport m
    -> KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec m a, OutSpecs)
    -> Production a
runRawKBasedMode unwrap wrap peerId transport kinst np@NodeParams {..} sscnp (ActionSpec action, outSpecs) =
    runRawRealMode
        peerId
        (hoistTransport hoistDown transport)
        np
        sscnp
        listeners
        outSpecs $
    ActionSpec
        $ \vI sendActions -> hoistDown . action vI $ hoistSendActions hoistUp hoistDown sendActions
  where
    hoistUp = wrap . lift
    hoistDown = runDiscoveryKademliaT kinst . unwrap
    listeners =
        hoistDown $
        first (hoistListenerSpec hoistDown hoistUp <$>) <$>
        allListeners

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
runProductionMode = runRawKBasedMode getNoStatsT lift

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
runStatsMode peerId transport kinst np sscnp action = do
    statMap <- liftIO SM.newIO
    runRawKBasedMode (runStatsT' statMap) lift peerId transport kinst np sscnp action

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
    hoistDown = runDiscoveryConstT peers . getNoStatsT
    listeners =
        hoistDown $
        first (hoistListenerSpec hoistDown hoistUp <$>) <$>
        allListeners

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH
    :: forall ssc m a.
       ( SscConstraint ssc
       , MonadIO m
       , MonadCatch m
       , Mockable CurrentTime m)
    => Int
    -> NodeParams
    -> SscNodeContext ssc
    -> NodeDBs
    -> Ether.ReadersT (NodeContext ssc) m a
    -> m a
runCH allWorkersNum params@NodeParams {..} sscNodeContext db act = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncJLFile <- JLFile <$>
        liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
    ncBlkSemaphore <- BlkSemaphore <$> newEmptyMVar
    ucUpdateSemaphore <- newEmptyMVar

    -- TODO [CSL-775] lrc initialization logic is duplicated.
    epochDef <- Ether.runReaderT' LrcDB.getEpochDefault db
    lcLrcSync <- newTVarIO (LrcSyncData True epochDef)

    let eternity = (minBound, maxBound)
        makeOwnPSK = flip (createProxySecretKey npSecretKey) eternity . encToPublic
        ownPSKs = npUserSecret ^.. usKeys._tail.each.to makeOwnPSK
    Ether.runReaderT' (for_ ownPSKs addProxySecretKey) db

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
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    let ctx =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
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
    Ether.runReadersT act ctx

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
