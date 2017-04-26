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
       , RealModeResources(..)
       ) where

import           Control.Concurrent.STM      (newEmptyTMVarIO, newTBQueueIO)
import           Control.Lens                (each, to, _tail)
import           Control.Monad.Fix           (MonadFix)
import           Data.Default                (def)
import qualified Data.Set                    as Set (fromList)
import           Data.Tagged                 (untag)
import qualified Data.Time                   as Time
import           Formatting                  (build, sformat, shown, (%))
import           Mockable                    (CurrentTime, Mockable, MonadMockable,
                                              Production (..), Throw, bracket, finally,
                                              throw)
import           Network.QDisc.Fair          (fairQDisc)
import           Network.Transport.Abstract  (Transport, closeTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import           Node                        (Node, NodeAction (..),
                                              defaultNodeEnvironment, hoistSendActions,
                                              node, simpleNodeEndPoint)
import           Node.Util.Monitor           (setupMonitor, stopMonitor)
import qualified STMContainers.Map           as SM
import           System.Random               (newStdGen)
import           System.Wlog                 (LoggerConfig (..), WithLogger, logError,
                                              logInfo, productionB, releaseAllHandlers,
                                              setupLogging, usingLoggerName)
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
import           Pos.DHT.Model               (dhtNodeToNodeId, randomDHTKey)
import           Pos.DHT.Real                (KademliaDHTInstance,
                                              KademliaDHTInstanceConfig (..),
                                              KademliaParams (..), kademliaGetKnownPeers,
                                              kdiHandle, lookupNode, startDHTInstance,
                                              stopDHTInstance)
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
import           Pos.Statistics              (getNoStatsT, runStatsT')
import           Pos.Txp                     (mkTxpLocalData, runTxpHolder)
import           Pos.Txp.DB                  (genesisFakeTotalStake,
                                              runBalanceIterBootstrap)
#ifdef WITH_EXPLORER
import           Pos.Explorer                (explorerTxpGlobalSettings)
#else
import           Pos.Txp                     (txpGlobalSettings)
#endif
import           Pos.Launcher.Resources      (RealModeResources (..), hoistResources)
import           Pos.Update.Context          (UpdateContext (..))
import qualified Pos.Update.DB               as GState
import           Pos.Update.MemState         (newMemVar)
import           Pos.Util.UserSecret         (usKeys)
import           Pos.Worker                  (allWorkersCount)
import           Pos.WorkMode                (ProductionMode, RawRealMode, ServiceMode,
                                              StatsMode)

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
    -> RealModeResources (RawRealMode ssc)
    -> NodeParams
    -> SscParams ssc
    -> RawRealMode ssc (ListenersWithOut (RawRealMode ssc))
    -> OutSpecs
    -> ActionSpec (RawRealMode ssc) a
    -> Production a
runRawRealMode peerId res np@NodeParams {..} sscnp listeners outSpecs (ActionSpec action) =
    usingLoggerName lpRunnerTag $ do
       initNC <- untag @ssc sscCreateNodeContext sscnp
       modernDBs <- openNodeDBs npRebuildDb npDbPathM
       let allWorkersNum = allWorkersCount @ssc @(ProductionMode ssc) :: Int
       -- TODO [CSL-775] ideally initialization logic should be in scenario.
       runCH @ssc allWorkersNum np initNC modernDBs $ initNodeDBs
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
                       runCH @ssc allWorkersNum np initNC modernDBs .
                       runSlottingHolder slottingVar .
                       runNtpSlotting ntpSlottingVar .
                       ignoreSscHolder .
                       runTxpHolder txpVar .
                       runDelegationT def .
                       runPeerStateHolder stateM_

       let startMonitoring node' = case lpEkgPort of
               Nothing   -> return Nothing
               Just port -> Just <$> setupMonitor port runIO node'

       let stopMonitoring it = whenJust it stopMonitor

       runCH allWorkersNum np initNC modernDBs .
          runSlottingHolder slottingVar .
          runNtpSlotting ntpSlottingVar .
          (mkStateAndRunSscHolder @ssc) .
          runTxpHolder txpVar .
          runDelegationT def .
          runPeerStateHolder stateM .
          runServer peerId (rmTransport res) listeners outSpecs startMonitoring stopMonitoring . ActionSpec $
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
    -> RealModeResources ServiceMode
    -> BaseParams
    -> ListenersWithOut ServiceMode
    -> OutSpecs
    -> ActionSpec ServiceMode a
    -> Production a
runServiceMode peerId res bp@BaseParams {..} listeners outSpecs (ActionSpec action) = do
    stateM <- liftIO SM.newIO
    usingLoggerName (lpRunnerTag bpLoggingParams) .
        runPeerStateHolder stateM .
        runServer_ peerId (rmTransport res) listeners outSpecs . ActionSpec $ \vI sa ->
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

-- | ProductionMode runner.
runProductionMode
    :: forall ssc a.
       (SscConstraint ssc)
    => PeerId
    -> RealModeResources (ProductionMode ssc)
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (ProductionMode ssc) a, OutSpecs)
    -> Production a
runProductionMode peerId res np@NodeParams {..} sscnp (ActionSpec action, outSpecs) =
    runRawRealMode peerId (hoistResources getNoStatsT res) np sscnp listeners outSpecs $ ActionSpec
        $ \vI sendActions -> getNoStatsT . action vI $ hoistSendActions lift getNoStatsT sendActions
  where
    listeners = getNoStatsT $
        first (hoistListenerSpec getNoStatsT lift <$>) <$> allListeners (rmGetPeers res)

-- | StatsMode runner.
-- [CSL-169]: spawn here additional listener, which would accept stat queries
-- can be done as part of refactoring (or someone who will refactor will create new issue).
runStatsMode
    :: forall ssc a.
       (SscConstraint ssc)
    => PeerId
    -> RealModeResources (StatsMode ssc)
    -> NodeParams
    -> SscParams ssc
    -> (ActionSpec (StatsMode ssc) a, OutSpecs)
    -> Production a
runStatsMode peerId res np@NodeParams {..} sscnp (ActionSpec action, outSpecs) = do
    statMap <- liftIO SM.newIO
    let listeners = runStatsT' statMap $
            first (hoistListenerSpec (runStatsT' statMap) lift <$>) <$> allListeners (rmGetPeers res)
    runRawRealMode peerId (hoistResources (runStatsT' statMap) res) np sscnp listeners outSpecs . ActionSpec $
        \vI sendActions ->
            runStatsT' statMap . action vI $ hoistSendActions lift (runStatsT' statMap) sendActions

----------------------------------------------------------------------------
-- Lower level runners
----------------------------------------------------------------------------

runCH :: forall ssc m a . (SscConstraint ssc, MonadIO m, MonadCatch m, Mockable CurrentTime m)
      => Int -> NodeParams -> SscNodeContext ssc -> NodeDBs -> DBHolder (ContextHolder ssc m) a -> m a
runCH allWorkersNum params@NodeParams {..} sscNodeContext db act = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncJLFile <- liftIO (maybe (pure Nothing) (fmap Just . newMVar) npJLFile)
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
    let ctx =
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

-- | Bracket a transport and use a static set of peers for discovery.
bracketResources
    :: BaseParams
    -> TCP.TCPAddr
    -> Set NodeId
    -> (RealModeResources Production -> Production a)
    -> IO a
bracketResources bp tcpAddr staticPeers action =
    loggerBracket (bpLoggingParams bp) .
    runProduction $
        -- Both the DHT and Transport are problematic here:
        -- 1. We assume you'll want a DHT.
        -- 2. We assume your transport takes an IP/port.
        let rmGetPeers = return staticPeers
            rmFindPeers = return mempty
        in  bracketTransport tcpAddr $ \rmTransport ->
                action $ RealModeResources {..}

-- | Bracket a transport and a Kademlia node, using the latter for discovery.
bracketResourcesKademlia
    :: BaseParams
    -> TCP.TCPAddr
    -> KademliaParams
    -> (KademliaDHTInstance -> RealModeResources Production -> Production a)
    -> IO a
bracketResourcesKademlia bp tcpAddr kp action =
    loggerBracket (bpLoggingParams bp) .
    runProduction .
    bracketDHTInstance bp kp $ \kademliaInstance ->
        let rmGetPeers = fmap (Set.fromList . fmap dhtNodeToNodeId) (kademliaGetKnownPeers kademliaInstance)
            rmFindPeers = do
              _ <- liftIO (randomDHTKey >>= lookupNode (kdiHandle kademliaInstance))
              rmGetPeers
        in  bracketTransport tcpAddr $ \rmTransport ->
                action kademliaInstance (RealModeResources {..})
