{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
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

import           Control.Concurrent.STM       (newEmptyTMVarIO, newTBQueueIO)
import           Control.Lens                 (each, to, _tail)
import           Control.Monad.Fix            (MonadFix)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Data.Conduit                 (runConduitRes, (.|))
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
import qualified System.Metrics               as Metrics
import           System.Random                (newStdGen)
import qualified System.Remote.Monitoring     as Monitoring
import           System.Wlog                  (LoggerConfig (..), WithLogger,
                                               getLoggerName, logDebug, logError, logInfo,
                                               productionB, releaseAllHandlers,
                                               setupLogging, usingLoggerName)
import           Universum                    hiding (bracket, finally)

import           Pos.Binary                   ()
import           Pos.Block.BListener          (runBListenerStub)
import           Pos.CLI                      (readLoggerConfig)
import           Pos.Communication            (ActionSpec (..), BiP (..), InSpecs (..),
                                               MkListeners (..), OutSpecs (..),
                                               VerInfo (..), allListeners,
                                               hoistMkListeners)
import           Pos.Communication.PeerState  (PeerStateTag, runPeerStateRedirect)
import qualified Pos.Constants                as Const
import           Pos.Context                  (BlkSemaphore (..), ConnectedPeers (..),
                                               NodeContext (..), StartTime (..))
import           Pos.Core                     (Timestamp ())
import           Pos.Crypto                   (createProxySecretKey, encToPublic)
import           Pos.DB                       (MonadDBRead, NodeDBs, runDBPureRedirect)
import           Pos.DB.Block                 (runBlockDBRedirect)
import           Pos.DB.DB                    (initNodeDBs, openNodeDBs,
                                               runGStateCoreRedirect)
import           Pos.DB.GState                (getTip)
import           Pos.DB.Misc                  (addProxySecretKey)
import           Pos.Delegation.Class         (DelegationVar)
import           Pos.DHT.Real                 (KademliaDHTInstance, KademliaParams (..),
                                               startDHTInstance, stopDHTInstance)
import           Pos.Discovery                (DiscoveryContextSum, runDiscoveryRedirect)
import           Pos.Genesis                  (genesisLeaders, genesisSeed)
import           Pos.Launcher.Param           (BaseParams (..), LoggingParams (..),
                                               NodeParams (..))
import           Pos.Lrc.Context              (LrcContext (..), LrcSyncData (..))
import qualified Pos.Lrc.DB                   as LrcDB
import           Pos.Lrc.Fts                  (followTheSatoshiM)
import           Pos.Security                 (SecurityWorkersClass)
import           Pos.Slotting                 (NtpSlottingVar, SlottingVar,
                                               mkNtpSlottingVar)
import           Pos.Slotting.MemState.Holder (runSlotsDataRedirect)
import           Pos.Slotting.Ntp             (runSlotsRedirect)
import           Pos.Ssc.Class                (SscConstraint, SscNodeContext, SscParams,
                                               sscCreateNodeContext)
import           Pos.Ssc.Extra                (SscMemTag, bottomSscState, mkSscState)
import           Pos.Txp                      (mkTxpLocalData)
import           Pos.Txp.DB                   (balanceSource, genesisFakeTotalStake)
import           Pos.Txp.MemState             (TxpHolderTag)
#ifdef WITH_EXPLORER
import           Pos.Explorer                 (explorerTxpGlobalSettings)
#else
import           Pos.Txp                      (txpGlobalSettings)
#endif
import           Pos.Update.Context           (UpdateContext (..))
import qualified Pos.Update.DB                as GState
import           Pos.Update.MemState          (newMemVar)
import           Pos.Util.Concurrent.RWVar    as RWV
import           Pos.Util.JsonLog             (JLFile (..))
import           Pos.Util.UserSecret          (usKeys)
import           Pos.Worker                   (allWorkersCount)
import           Pos.WorkMode                 (RealMode (..), ServiceMode (..), WorkMode)

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
runRealModeDo discoveryCtx transport np@NodeParams {..} sscnp
              listeners outSpecs (ActionSpec action) =
    usingLoggerName lpRunnerTag $ do
        initNC <- untag @ssc sscCreateNodeContext sscnp
        modernDBs <- openNodeDBs npRebuildDb npDbPathM
        let allWorkersNum = allWorkersCount @ssc @(RealMode ssc) :: Int
        let runCHHere = runCH @ssc allWorkersNum discoveryCtx np initNC modernDBs
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

        -- TODO [CSL-775] need an effect-free way of running this into IO.
        let runIO :: forall t . RealMode ssc t -> IO t
            runIO (RealMode act) =
               runProduction .
                   usingLoggerName lpRunnerTag .
                   runCHHere .
                   flip Ether.runReadersT
                      ( Tagged @NodeDBs modernDBs
                      , Tagged @SlottingVar slottingVar
                      , Tagged @(Bool, NtpSlottingVar) (npUseNTP, ntpSlottingVar)
                      , Tagged @SscMemTag bottomSscState
                      , Tagged @TxpHolderTag txpVar
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

        ekgStore <- liftIO $ Metrics.newStore
        -- To start monitoring, add the time-warp metrics and the GC
        -- metrics then spin up the server.
        let startMonitoring node' = case lpEkgPort of
                Nothing   -> return Nothing
                Just port -> Just <$> do
                     ekgStore' <- setupMonitor runIO node' ekgStore
                     liftIO $ Metrics.registerGcMetrics ekgStore'
                     liftIO $ Monitoring.forkServerWith ekgStore' "127.0.0.1" port

        let stopMonitoring it = whenJust it stopMonitor

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
               , Tagged @TxpHolderTag txpVar
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
           runServer transport listeners outSpecs startMonitoring stopMonitoring . ActionSpec $
               \vI sa -> nodeStartMsg npBaseParams >> action vI sa
  where
    LoggingParams {..} = bpLoggingParams npBaseParams
{-# NOINLINE runRealMode #-}

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
    usingLoggerName (lpRunnerTag bpLoggingParams) .
        flip (Ether.runReaderT @PeerStateTag) stateM .
        runPeerStateRedirect .
        (\(ServiceMode m) -> m) .
        runServer_ transport listeners outSpecs . ActionSpec $ \vI sa ->
        nodeStartMsg bp >> action vI sa
{-# NOINLINE runServiceMode #-}

runServer
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m
    -> MkListeners m
    -> OutSpecs
    -> (Node m -> m t)
    -> (t -> m ())
    -> ActionSpec m b
    -> m b
runServer transport mkL (OutSpecs wouts) withNode afterNode (ActionSpec action) = do
    stdGen <- liftIO newStdGen
    logInfo $ sformat ("Our verInfo: "%build) ourVerInfo
    node (simpleNodeEndPoint transport) (const $ pure Nothing) stdGen BiP ourVerInfo defaultNodeEnvironment $ \__node ->
        NodeAction mkListeners' $ \sendActions -> do
            t <- withNode __node
            action ourVerInfo sendActions `finally` afterNode t
  where
    InSpecs ins = inSpecs mkL
    OutSpecs outs = outSpecs mkL
    ourVerInfo =
        VerInfo Const.protocolMagic Const.lastKnownBlockVersion ins $ outs <> wouts
    mkListeners' theirVerInfo = do
        logDebug $ sformat ("Incoming connection: theirVerInfo="%build) theirVerInfo
        mkListeners mkL ourVerInfo theirVerInfo

runServer_
    :: (MonadIO m, MonadMockable m, MonadFix m, WithLogger m)
    => Transport m -> MkListeners m -> OutSpecs -> ActionSpec m b -> m b
runServer_ transport mkl outSpecs =
    runServer transport mkl outSpecs acquire release
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
       , MonadBaseControl IO m
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
    ncJLFile <- JLFile <$>
        liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
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
    ncGenesisLeaders <-
        if Const.isDevelopment
        then pure $ genesisLeaders npCustomUtxo
        else flip Ether.runReaderT' db $ runDBPureRedirect $
             runConduitRes $
             balanceSource .| followTheSatoshiM genesisSeed genesisFakeTotalStake
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
