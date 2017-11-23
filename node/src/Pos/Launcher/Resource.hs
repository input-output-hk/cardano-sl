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
       , bracketKademlia
       , bracketTransport
       ) where

import           Universum                  hiding (bracket, finally)

import           Control.Concurrent.STM     (newEmptyTMVarIO, newTBQueueIO)
import           Data.Tagged                (untag)
import qualified Data.Time                  as Time
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Bracket, Catch, Mockable, Production (..),
                                             Throw, bracket, throw)
import           Network.QDisc.Fair         (fairQDisc)
import qualified Network.Transport          as NT (closeTransport)
import           Network.Transport.Abstract (Transport, hoistTransport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP      as TCP
import           System.IO                  (BufferMode (..), Handle, hClose,
                                             hSetBuffering)
import qualified System.Metrics             as Metrics
import           System.Wlog                (CanLog, LoggerConfig (..), WithLogger,
                                             getLoggerName, logError, prefixB,
                                             productionB, releaseAllHandlers,
                                             setupLogging, showTidB, usingLoggerName)

import           Pos.Binary                 ()
import           Pos.Block.Slog             (mkSlogContext)
import           Pos.Client.CLI.Util        (readLoggerConfig)
import           Pos.Configuration
import           Pos.Context                (ConnectedPeers (..), NodeContext (..),
                                             StartTime (..))
import           Pos.Core                   (HasConfiguration, Timestamp,
                                             gdStartTime, genesisData)
import           Pos.DB                     (MonadDBRead, NodeDBs)
import           Pos.DB.DB                  (initNodeDBs)
import           Pos.DB.Rocks               (closeNodeDBs, openNodeDBs)
import           Pos.Delegation             (DelegationVar, mkDelegationVar)
import           Pos.DHT.Real               (KademliaDHTInstance, KademliaParams (..),
                                             startDHTInstance, stopDHTInstance)
import           Pos.Infra.Configuration    (HasInfraConfiguration)
import           Pos.Launcher.Param         (BaseParams (..), LoggingParams (..),
                                             NodeParams (..), TransportParams (..))
import           Pos.Lrc.Context            (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types          (NetworkConfig (..), Topology (..))
import           Pos.Shutdown.Types         (ShutdownContext (..))
import           Pos.Slotting               (SlottingContextSum (..), SlottingData,
                                             mkNtpSlottingVar, mkSimpleSlottingVar)
import           Pos.Ssc.Class              (SscConstraint, SscParams,
                                             sscCreateNodeContext)
import           Pos.Ssc.Extra              (SscState, mkSscState)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.StateLock                    (newStateLock)
import           Pos.Txp                          (GenericTxpLocalData (..),
                                                   mkTxpLocalData, recordTxpMetrics)
#ifdef WITH_EXPLORER
import           Pos.Explorer                     (explorerTxpGlobalSettings)
#else
import           Pos.Txp                          (txpGlobalSettings)
#endif

import           Pos.Launcher.Mode                (InitMode, InitModeContext (..),
                                                   runInitMode)
import           Pos.Update.Context               (mkUpdateContext)
import qualified Pos.Update.DB                    as GState
import           Pos.Util                         (newInitFuture)
import           Pos.WorkMode                     (TxpExtra_TMP)

#ifdef linux_HOST_OS
import qualified System.Systemd.Daemon            as Systemd
import qualified System.Wlog                      as Logger
#endif

-- Remove this once there's no #ifdef-ed Pos.Txp import
{-# ANN module ("HLint: ignore Use fewer imports" :: Text) #-}

----------------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------------

-- | This data type contains all resources used by node.
data NodeResources ssc m = NodeResources
    { nrContext    :: !(NodeContext ssc)
    , nrDBs        :: !NodeDBs
    , nrSscState   :: !(SscState ssc)
    , nrTxpState   :: !(GenericTxpLocalData TxpExtra_TMP)
    , nrDlgState   :: !DelegationVar
    , nrTransport  :: !(Transport m)
    , nrJLogHandle :: !(Maybe Handle)
    -- ^ Handle for JSON logging (optional).
    , nrEkgStore   :: !Metrics.Store
    }

hoistNodeResources ::
       forall ssc n m. Functor m
    => (forall a. n a -> m a)
    -> NodeResources ssc n
    -> NodeResources ssc m
hoistNodeResources nat nr =
    nr {nrTransport = hoistTransport nat (nrTransport nr)}

----------------------------------------------------------------------------
-- Allocation/release/bracket
----------------------------------------------------------------------------

-- | Allocate all resources used by node. They must be released eventually.
allocateNodeResources
    :: forall ssc m.
       ( SscConstraint ssc
       , HasConfiguration
       , HasNodeConfiguration
       , HasInfraConfiguration
       -- FIXME avieth
       -- 'HasGtConfiguration' arises from 'initNodeDBs', where that constraint
       -- in turn arises from 'prepareGStateDB', which is in fact tied to
       -- godtossing.
       -- So the 'forall ssc' here is misleading. This only works for
       -- godtossing. The dependency was hidden before, where the godtossing
       -- data was all delivered by global mutable variables. That's to say,
       -- 'allocateNodeResources' had a hidden assumption that somebody will
       -- fill in the required godtossing data, probably using 'unsafePerformIO'.
       , HasGtConfiguration
       )
    => Transport m
    -> NetworkConfig KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> Production (NodeResources ssc m)
allocateNodeResources transport networkConfig np@NodeParams {..} sscnp = do
    db <- openNodeDBs npRebuildDb npDbPathM
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
        initNodeDBs @ssc

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
        ctx@NodeContext {..} <- allocateNodeContext ancd
        putLrcContext ncLrcContext
        dlgVar <- mkDelegationVar @ssc
        sscState <- mkSscState @ssc
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
       forall ssc m. ( )
    => NodeResources ssc m -> Production ()
releaseNodeResources NodeResources {..} = do
    whenJust nrJLogHandle (liftIO . hClose)
    closeNodeDBs nrDBs
    releaseNodeContext nrContext

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ssc m a.
      ( SscConstraint ssc
      , MonadIO m
      , HasConfiguration
      , HasNodeConfiguration
      , HasInfraConfiguration
      , HasGtConfiguration
      )
    => NodeParams
    -> SscParams ssc
    -> (HasConfiguration => NodeResources ssc m -> Production a)
    -> Production a
bracketNodeResources np sp k = bracketTransport tcpAddr $ \transport ->
    bracketKademlia (npBaseParams np) (npNetworkConfig np) $ \networkConfig ->
        bracket (allocateNodeResources transport networkConfig np sp) releaseNodeResources $ \nodeRes ->do
            -- Notify systemd we are fully operative
            notifyReady
            k nodeRes
  where
    tcpAddr = tpTcpAddr (npTransport np)

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    let cfgBuilder = productionB
                  <> showTidB
                  <> maybe mempty prefixB lpHandlerPrefix
    cfg <- readLoggerConfig lpConfigPath
    pure $ cfg <> cfgBuilder

setupLoggers :: MonadIO m => LoggingParams -> m ()
setupLoggers params = setupLogging =<< getRealLoggerConfig params

-- | RAII for Logging.
loggerBracket :: LoggingParams -> IO a -> IO a
loggerBracket lp = bracket_ (setupLoggers lp) releaseAllHandlers

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data AllocateNodeContextData ssc = AllocateNodeContextData
    { ancdNodeParams :: !NodeParams
    , ancdSscParams :: !(SscParams ssc)
    , ancdPutSlotting :: (Timestamp, TVar SlottingData) -> SlottingContextSum -> InitMode ssc ()
    , ancdNetworkCfg :: NetworkConfig KademliaDHTInstance
    , ancdEkgStore :: !Metrics.Store
    , ancdTxpMemState :: !(GenericTxpLocalData TxpExtra_TMP)
    }

allocateNodeContext
    :: forall ssc .
      (HasConfiguration, HasNodeConfiguration, HasInfraConfiguration, SscConstraint ssc)
    => AllocateNodeContextData ssc
    -> InitMode ssc (NodeContext ssc)
allocateNodeContext ancd = do
    let AllocateNodeContextData { ancdNodeParams = np@NodeParams {..}
                                , ancdSscParams = sscnp
                                , ancdPutSlotting = putSlotting
                                , ancdNetworkCfg = networkConfig
                                , ancdEkgStore = store
                                , ancdTxpMemState = TxpLocalData {..}
                                } = ancd
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncStateLock <- newStateLock
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
    ncSscContext <- untag @ssc sscCreateNodeContext sscnp
    ncSlogContext <- mkSlogContext store
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    let ctx shutdownQueue =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
            , ncLrcContext = LrcContext {..}
            , ncShutdownContext = ShutdownContext ncShutdownFlag shutdownQueue
            , ncNodeParams = np
#ifdef WITH_EXPLORER
            , ncTxpGlobalSettings = explorerTxpGlobalSettings
#else
            , ncTxpGlobalSettings = txpGlobalSettings
#endif
            , ncNetworkConfig = networkConfig
            , ..
            }
    -- TODO bounded queue not necessary.
    ctx <$> liftIO (newTBQueueIO maxBound)

releaseNodeContext :: forall ssc m . MonadIO m => NodeContext ssc -> m ()
releaseNodeContext _ = return ()

-- Create new 'SlottingVar' using data from DB. Probably it would be
-- good to have it in 'infra', but it's complicated.
mkSlottingVar :: (MonadIO m, MonadDBRead m) => m (TVar SlottingData)
mkSlottingVar = newTVarIO =<< GState.getSlottingData

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

createKademliaInstance ::
       (HasNodeConfiguration, MonadIO m, Mockable Catch m, Mockable Throw m, CanLog m)
    => BaseParams
    -> KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> m KademliaDHTInstance
createKademliaInstance BaseParams {..} kp defaultPort =
    usingLoggerName (lpRunnerTag bpLoggingParams) (startDHTInstance instConfig defaultBindAddress)
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp ++ defaultPeers}
    defaultBindAddress = ("0.0.0.0", defaultPort)

-- | RAII for 'KademliaDHTInstance'.
bracketKademliaInstance
    :: (HasNodeConfiguration, MonadIO m, Mockable Catch m, Mockable Throw m, Mockable Bracket m, CanLog m)
    => BaseParams
    -> KademliaParams
    -> Word16 -- ^ Default port to bind to.
    -> (KademliaDHTInstance -> m a)
    -> m a
bracketKademliaInstance bp kp defaultPort action =
    bracket (createKademliaInstance bp kp defaultPort) stopDHTInstance action

-- | The 'NodeParams' contain enough information to determine whether a Kademlia
-- instance should be brought up. Use this to safely acquire/release one.
bracketKademlia
    :: (HasNodeConfiguration, MonadIO m, Mockable Catch m, Mockable Throw m, Mockable Bracket m, CanLog m)
    => BaseParams
    -> NetworkConfig KademliaParams
    -> (NetworkConfig KademliaDHTInstance -> m a)
    -> m a
bracketKademlia bp nc@NetworkConfig {..} action = case ncTopology of
    -- cases that need Kademlia
    TopologyP2P{topologyKademlia = kp, ..} ->
      bracketKademliaInstance bp kp ncDefaultPort $ \kinst ->
        k $ TopologyP2P{topologyKademlia = kinst, ..}
    TopologyTraditional{topologyKademlia = kp, ..} ->
      bracketKademliaInstance bp kp ncDefaultPort $ \kinst ->
        k $ TopologyTraditional{topologyKademlia = kinst, ..}
    TopologyRelay{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance bp kp ncDefaultPort $ \kinst ->
        k $ TopologyRelay{topologyOptKademlia = Just kinst, ..}
    TopologyCore{topologyOptKademlia = Just kp, ..} ->
      bracketKademliaInstance bp kp ncDefaultPort $ \kinst ->
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
    :: (HasNodeConfiguration, MonadIO n, MonadIO m, WithLogger m, Mockable Throw m)
    => TCP.TCPAddr
    -> m (Transport n, m ())
createTransportTCP addrInfo = do
    loggerName <- getLoggerName
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
            throw e
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)

-- | RAII for 'Transport'.
bracketTransport
    :: (HasNodeConfiguration, MonadIO m, MonadIO n, Mockable Throw m, Mockable Bracket m, WithLogger m)
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
