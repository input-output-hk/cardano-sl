{-# LANGUAGE CPP           #-}
{-# LANGUAGE Rank2Types    #-}
{-# LANGUAGE TypeOperators #-}

-- | Resources used by node and ways to deal with them.

module Pos.Launcher.Resource
       (
         -- * Full resources
         NodeResources (..)
       , hoistNodeResources

       , allocateNodeResources
       , releaseNodeResources
       , bracketNodeResources

         -- * Smaller resources
       , loggerBracket
       , bracketTransport
       ) where

import           Nub (ordNub)
import           Universum

import           Control.Concurrent.STM (newEmptyTMVarIO, newTBQueueIO)
import           Data.Default (Default)
import qualified Data.Time as Time
import           Data.Time.Units (toMicroseconds)
import           Formatting (sformat, shown, (%))
import           Mockable (Production (..))
import           Network.QDisc.Fair (fairQDisc)
import qualified Network.Transport as NT (closeTransport)
import           Network.Transport.Abstract (Transport, hoistTransport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP as TCP
import           System.IO (BufferMode (..), Handle, hClose, hSetBuffering)
import qualified System.Metrics as Metrics
import           System.Wlog (CanLog, LoggerConfig (..), WithLogger, askLoggerName, consoleActionB,
                              defaultHandleAction, logError, logInfo, maybeLogsDirB, productionB,
                              removeAllHandlers, setupLogging, showTidB, usingLoggerName)

import           Pos.Binary ()
import           Pos.Block.Slog (mkSlogContext)
import           Pos.Client.CLI.Util (readLoggerConfig)
import           Pos.Configuration
import           Pos.Context (ConnectedPeers (..), NodeContext (..), StartTime (..))
import           Pos.Core (HasConfiguration, Timestamp, bvdSlotDuration, gdBlockVersionData,
                           gdStartTime, genesisData)
import           Pos.DB (MonadDBRead, NodeDBs)
import           Pos.DB.Rocks (closeNodeDBs, openNodeDBs)
import           Pos.Delegation (DelegationVar, HasDlgConfiguration, mkDelegationVar)
import           Pos.DHT.Real (KademliaDHTInstance, KademliaParams (..), startDHTInstance,
                               stopDHTInstance)
import qualified Pos.GState as GS
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Lrc.Context (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types (NetworkConfig (..), Topology (..))
import           Pos.Reporting.MemState (initializeMisbehaviorMetrics)
import           Pos.Shutdown.Types (ShutdownContext (..))
import           Pos.Slotting (SlottingContextSum (..), mkNtpSlottingVar, mkSimpleSlottingVar)
import           Pos.Slotting.Types (SlottingData)
import           Pos.Ssc (HasSscConfiguration, SscParams, SscState, createSscContext, mkSscState)
import           Pos.StateLock (newStateLock)
import           Pos.Txp (GenericTxpLocalData (..), TxpGlobalSettings, mkTxpLocalData,
                          recordTxpMetrics)

import           Pos.Launcher.Mode (InitMode, InitModeContext (..), runInitMode)
import           Pos.Update.Context (mkUpdateContext)
import qualified Pos.Update.DB as GState
import           Pos.Util (newInitFuture)
import           Pos.Util.Timer (newTimer)

#ifdef linux_HOST_OS
import qualified System.Systemd.Daemon as Systemd
import qualified System.Wlog as Logger
#endif

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------------

-- | This data type contains all resources used by node.
data NodeResources ext m = NodeResources
    { nrContext    :: !NodeContext
    , nrDBs        :: !NodeDBs
    , nrSscState   :: !SscState
    , nrTxpState   :: !(GenericTxpLocalData ext)
    , nrDlgState   :: !DelegationVar
    , nrTransport  :: !(Transport m)
    , nrJLogHandle :: !(Maybe Handle)
    -- ^ Handle for JSON logging (optional).
    , nrEkgStore   :: !Metrics.Store
    }

hoistNodeResources ::
       forall ext n m. Functor m
    => (forall a. n a -> m a)
    -> NodeResources ext n
    -> NodeResources ext m
hoistNodeResources nat nr =
    nr {nrTransport = hoistTransport nat (nrTransport nr)}

----------------------------------------------------------------------------
-- Allocation/release/bracket
----------------------------------------------------------------------------

-- | Allocate all resources used by node. They must be released eventually.
allocateNodeResources
    :: forall ext m.
       ( Default ext
       , HasConfiguration
       , HasNodeConfiguration
       , HasInfraConfiguration
       , HasSscConfiguration
       , HasDlgConfiguration
       )
    => Transport m
    -> NetworkConfig KademliaDHTInstance
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> Production (NodeResources ext m)
allocateNodeResources transport networkConfig np@NodeParams {..} sscnp txpSettings initDB = do
    npDbPath <- case npDbPathM of
        Nothing -> do
            let dbPath = "node-db" :: FilePath
            logInfo $ sformat ("DB path not specified, defaulting to "%
                               shown) dbPath
            return dbPath
        Just dbPath -> return dbPath
    db <- openNodeDBs npRebuildDb npDbPath
    (futureLrcContext, putLrcContext) <- newInitFuture "lrcContext"
    (futureSlottingVar, putSlottingVar) <- newInitFuture "slottingVar"
    (futureSlottingContext, putSlottingContext) <- newInitFuture "slottingContext"
    let putSlotting sv sc = do
            putSlottingVar sv
            putSlottingContext sc
        initModeContext = InitModeContext
            db
            futureSlottingVar
            futureSlottingContext
            futureLrcContext
    runInitMode initModeContext $ do
        initDB

        nrEkgStore <- liftIO $ Metrics.newStore

        txpVar <- mkTxpLocalData -- doesn't use slotting or LRC
        let ancd =
                AllocateNodeContextData
                { ancdNodeParams = np
                , ancdSscParams = sscnp
                , ancdPutSlotting = putSlotting
                , ancdNetworkCfg = networkConfig
                , ancdEkgStore = nrEkgStore
                , ancdTxpMemState = txpVar
                }
        ctx@NodeContext {..} <- allocateNodeContext ancd txpSettings nrEkgStore
        putLrcContext ncLrcContext
        dlgVar <- mkDelegationVar
        sscState <- mkSscState
        let nrTransport = transport
        nrJLogHandle <-
            case npJLFile of
                Nothing -> pure Nothing
                Just fp -> do
                    h <- openFile fp WriteMode
                    liftIO $ hSetBuffering h NoBuffering
                    return $ Just h

        return NodeResources
            { nrContext = ctx
            , nrDBs = db
            , nrSscState = sscState
            , nrTxpState = txpVar
            , nrDlgState = dlgVar
            , ..
            }

-- | Release all resources used by node. They must be released eventually.
releaseNodeResources ::
       NodeResources ext m -> Production ()
releaseNodeResources NodeResources {..} = do
    whenJust nrJLogHandle (liftIO . hClose)
    closeNodeDBs nrDBs
    releaseNodeContext nrContext

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ext m a.
      ( Default ext
      , MonadIO m
      , HasConfiguration
      , HasNodeConfiguration
      , HasInfraConfiguration
      , HasSscConfiguration
      , HasDlgConfiguration
      )
    => NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> (HasConfiguration => NodeResources ext m -> Production a)
    -> Production a
bracketNodeResources np sp txp initDB action =
    bracketTransport (ncTcpAddr (npNetworkConfig np)) $ \transport ->
        bracketKademlia (npNetworkConfig np) $ \networkConfig ->
            bracket (allocateNodeResources transport networkConfig np sp txp initDB)
                    releaseNodeResources $ \nodeRes ->do
                -- Notify systemd we are fully operative
                notifyReady
                action nodeRes

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    let cfgBuilder = productionB
                  <> showTidB
                  <> maybeLogsDirB lpHandlerPrefix
    cfg <- readLoggerConfig lpConfigPath
    pure $ overrideConsoleLog $ cfg <> cfgBuilder
  where
    overrideConsoleLog :: LoggerConfig -> LoggerConfig
    overrideConsoleLog = case lpConsoleLog of
        Nothing    -> identity
        Just True  -> (<>) (consoleActionB defaultHandleAction)
        Just False -> (<>) (consoleActionB (\_ _ -> pass))

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers params = setupLogging Nothing =<< getRealLoggerConfig params

-- | RAII for Logging.
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) removeAllHandlers

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data AllocateNodeContextData ext = AllocateNodeContextData
    { ancdNodeParams  :: !NodeParams
    , ancdSscParams   :: !SscParams
    , ancdPutSlotting :: (Timestamp, TVar SlottingData) -> SlottingContextSum -> InitMode ()
    , ancdNetworkCfg  :: NetworkConfig KademliaDHTInstance
    , ancdEkgStore    :: !Metrics.Store
    , ancdTxpMemState :: !(GenericTxpLocalData ext)
    }

allocateNodeContext
    :: forall ext .
      (HasConfiguration, HasNodeConfiguration, HasInfraConfiguration)
    => AllocateNodeContextData ext
    -> TxpGlobalSettings
    -> Metrics.Store
    -> InitMode NodeContext
allocateNodeContext ancd txpSettings ekgStore = do
    let AllocateNodeContextData { ancdNodeParams = np@NodeParams {..}
                                , ancdSscParams = sscnp
                                , ancdPutSlotting = putSlotting
                                , ancdNetworkCfg = networkConfig
                                , ancdEkgStore = store
                                , ancdTxpMemState = TxpLocalData {..}
                                } = ancd
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncStateLock <- newStateLock =<< GS.getTip
    ncStateLockMetrics <- liftIO $ recordTxpMetrics store txpMemPool
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    ncSlottingVar <- (gdStartTime genesisData,) <$> mkSlottingVar
    ncSlottingContext <-
        case npUseNTP of
            True  -> SCNtp <$> mkNtpSlottingVar
            False -> SCSimple <$> mkSimpleSlottingVar
    putSlotting ncSlottingVar ncSlottingContext
    ncUserSecret <- newTVarIO $ npUserSecret
    ncBlockRetrievalQueue <- liftIO $ newTBQueueIO blockRetrievalQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    ncProgressHeader <- liftIO newEmptyTMVarIO
    ncShutdownFlag <- newTVarIO False
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    ncUpdateContext <- mkUpdateContext
    ncSscContext <- createSscContext sscnp
    ncSlogContext <- mkSlogContext store
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    let slotDuration :: Integer
        slotDuration = toMicroseconds . bvdSlotDuration $ gdBlockVersionData genesisData
    ncSubscriptionKeepAliveTimer <- newTimer $ 3 * fromIntegral slotDuration
    mm <- initializeMisbehaviorMetrics ekgStore

    let ctx =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
            , ncLrcContext = LrcContext {..}
            , ncShutdownContext = ShutdownContext ncShutdownFlag
            , ncNodeParams = np
            , ncTxpGlobalSettings = txpSettings
            , ncNetworkConfig = networkConfig
            , ncMisbehaviorMetrics = Just mm
            , ..
            }
    return ctx

releaseNodeContext :: forall m . MonadIO m => NodeContext -> m ()
releaseNodeContext _ = return ()

-- Create new 'SlottingVar' using data from DB. Probably it would be
-- good to have it in 'infra', but it's complicated.
mkSlottingVar :: (MonadIO m, MonadDBRead m) => m (TVar SlottingData)
mkSlottingVar = newTVarIO =<< GState.getSlottingData

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

createKademliaInstance ::
       (HasNodeConfiguration, MonadIO m, MonadCatch m, CanLog m)
    => KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> m KademliaDHTInstance
createKademliaInstance kp defaultPort =
    usingLoggerName "kademlia" (startDHTInstance instConfig defaultBindAddress)
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp ++ defaultPeers}
    defaultBindAddress = ("0.0.0.0", defaultPort)

-- | RAII for 'KademliaDHTInstance'.
bracketKademliaInstance
    :: (HasNodeConfiguration, MonadIO m, MonadMask m, CanLog m)
    => KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> (KademliaDHTInstance -> m a)
    -> m a
bracketKademliaInstance kp defaultPort action =
    bracket (createKademliaInstance kp defaultPort) stopDHTInstance action

-- | The 'NodeParams' contain enough information to determine whether a Kademlia
-- instance should be brought up. Use this to safely acquire/release one.
bracketKademlia
    :: (HasNodeConfiguration, MonadIO m, MonadMask m, CanLog m)
    => NetworkConfig KademliaParams
    -> (NetworkConfig KademliaDHTInstance -> m a)
    -> m a
bracketKademlia nc@NetworkConfig {..} action = case ncTopology of
    -- cases that need Kademlia
    TopologyP2P{topologyKademlia = kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyP2P{topologyKademlia = kinst, ..}
    TopologyTraditional{topologyKademlia = kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyTraditional{topologyKademlia = kinst, ..}
    TopologyRelay{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyRelay{topologyOptKademlia = Just kinst, ..}
    TopologyCore{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance kp ncDefaultPort $ \kinst ->
        k $ TopologyCore{topologyOptKademlia = Just kinst, ..}

    -- cases that don't
    TopologyRelay{topologyOptKademlia = Nothing, ..} ->
        k $ TopologyRelay{topologyOptKademlia = Nothing, ..}
    TopologyCore{topologyOptKademlia = Nothing, ..} ->
        k $ TopologyCore{topologyOptKademlia = Nothing, ..}
    TopologyBehindNAT{..} ->
        k $ TopologyBehindNAT{..}
    TopologyAuxx{..} ->
        k $ TopologyAuxx{..}
  where
    k topology = action (nc { ncTopology = topology })

data MissingKademliaParams = MissingKademliaParams
    deriving (Show)

instance Exception MissingKademliaParams

----------------------------------------------------------------------------
-- Transport
----------------------------------------------------------------------------

createTransportTCP
    :: (HasNodeConfiguration, MonadIO n, MonadIO m, WithLogger m, MonadThrow m)
    => TCP.TCPAddr
    -> m (Transport n, m ())
createTransportTCP addrInfo = do
    loggerName <- askLoggerName
    let tcpParams =
            (TCP.defaultTCPParameters
             { TCP.transportConnectTimeout =
                   Just $ fromIntegral networkConnectionTimeout
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
            throwM e
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)

-- | RAII for 'Transport'.
bracketTransport
    :: (HasNodeConfiguration, MonadIO m, MonadIO n, MonadMask m, WithLogger m)
    => TCP.TCPAddr
    -> (Transport n -> m a)
    -> m a
bracketTransport tcpAddr k =
    bracket (createTransportTCP tcpAddr) snd (k . fst)

-- | Notify process manager tools like systemd the node is ready.
-- Available only on Linux for systems where `libsystemd-dev` is installed.
-- It defaults to a noop for all the other platforms.
notifyReady :: (MonadIO m, WithLogger m) => m ()
#ifdef linux_HOST_OS
notifyReady = do
    res <- liftIO Systemd.notifyReady
    case res of
        Just () -> return ()
        Nothing -> Logger.logWarning "notifyReady failed to notify systemd."
#else
notifyReady = return ()
#endif
