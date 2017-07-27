{-# LANGUAGE CPP                 #-}
{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

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
import           Data.Tagged                 (untag)
import qualified Data.Time                  as Time
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Catch, Mockable, Production (..), Throw,
                                             throw, Bracket, bracket, MonadMockable)
import           Network.QDisc.Fair         (fairQDisc)
import           Network.Transport.Abstract (Transport, hoistTransport)
import           Network.Transport.Concrete (concrete)
import qualified Network.Transport.TCP      as TCP
import qualified Network.Transport          as NT (closeTransport)
import           System.IO                  (Handle, hClose)
import qualified System.Metrics             as Metrics
import           System.Wlog                (CanLog, LoggerConfig (..), WithLogger,
                                             getLoggerName, logError, productionB,
                                             releaseAllHandlers, setupLogging,
                                             usingLoggerName)

import           Pos.Binary                 ()
import           Pos.Block.Slog             (mkSlogContext)
import           Pos.CLI                    (readLoggerConfig)
import qualified Pos.Constants              as Const
import           Pos.Context                (BlkSemaphore (..), ConnectedPeers (..),
                                             NodeContext (..), StartTime (..))
import           Pos.Core                   (Timestamp)
import           Pos.DB                     (MonadDBRead, NodeDBs)
import           Pos.DB.DB                  (initNodeDBs)
import           Pos.DB.Rocks               (closeNodeDBs, openNodeDBs)
import           Pos.Delegation             (DelegationVar, mkDelegationVar)
import           Pos.DHT.Real               (KademliaDHTInstance, KademliaParams (..),
                                             startDHTInstance, stopDHTInstance)
import           Pos.Launcher.Param         (BaseParams (..), LoggingParams (..),
                                             TransportParams (..), NodeParams (..))
import           Pos.Lrc.Context            (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types          (NetworkConfig (..), Topology (..), NodeType (..))
import           Pos.Shutdown.Types         (ShutdownContext (..))
import           Pos.Slotting               (SlottingContextSum (..), SlottingData,
                                             mkNtpSlottingVar, mkSimpleSlottingVar)
import           Pos.Ssc.Class              (SscConstraint, SscParams,
                                             sscCreateNodeContext)
import           Pos.Ssc.Extra              (SscState, mkSscState)
import           Pos.Txp                    (GenericTxpLocalData, TxpMetrics,
                                             mkTxpLocalData, recordTxpMetrics)
#ifdef WITH_EXPLORER
import           Pos.Explorer               (explorerTxpGlobalSettings)
#else
import           Pos.Txp                    (txpGlobalSettings)
#endif

import           Pos.Launcher.Mode          (InitMode, InitModeContext (..),
                                             newInitFuture, runInitMode)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Update.Context         (mkUpdateContext)
import qualified Pos.Update.DB              as GState
import           Pos.WorkMode               (TxpExtra_TMP)

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
    , nrTxpState   :: !(GenericTxpLocalData TxpExtra_TMP, TxpMetrics)
    , nrDlgState   :: !DelegationVar
    , nrTransport  :: !(Transport m)
    , nrJLogHandle :: !(Maybe Handle)
    -- ^ Handle for JSON logging (optional).
    , nrEkgStore   :: !Metrics.Store
    , nrKademlia   :: !(Maybe KademliaDHTInstance)
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
       , SecurityWorkersClass ssc
       , WithLogger m
       , MonadIO m
       , MonadMockable m
       )
    => Transport m
    -> Maybe KademliaDHTInstance
    -> NodeParams
    -> SscParams ssc
    -> Production (NodeResources ssc m)
allocateNodeResources transport mKademlia np@NodeParams {..} sscnp = do
    db <- openNodeDBs npRebuildDb npDbPathM
    (futureLrcContext, putLrcContext) <- newInitFuture
    (futureSlottingVar, putSlottingVar) <- newInitFuture
    (futureSlottingContext, putSlottingContext) <- newInitFuture
    let putSlotting sv sc = do
            putSlottingVar sv
            putSlottingContext sc
        initModeContext = InitModeContext
            db
            npGenesisUtxo
            futureSlottingVar
            futureSlottingContext
            futureLrcContext
    runInitMode initModeContext $ do
        initNodeDBs @ssc
        ctx@NodeContext {..} <- allocateNodeContext np sscnp putSlotting
        putLrcContext ncLrcContext
        setupLoggers $ bpLoggingParams npBaseParams
        dlgVar <- mkDelegationVar @ssc
        txpVar <- mkTxpLocalData
        sscState <- mkSscState @ssc
        let nrTransport = transport
        nrJLogHandle <-
            case npJLFile of
                Nothing -> pure Nothing
                Just fp -> Just <$> openFile fp WriteMode

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
        nrEkgStore <- liftIO $ Metrics.newStore
        txpMetrics <- liftIO $ recordTxpMetrics nrEkgStore

        return NodeResources
            { nrContext = ctx
            , nrDBs = db
            , nrSscState = sscState
            , nrTxpState = (txpVar, txpMetrics)
            , nrDlgState = dlgVar
            , nrKademlia = mKademlia
            , ..
            }

-- | Release all resources used by node. They must be released eventually.
releaseNodeResources ::
       forall ssc m. (SscConstraint ssc, MonadIO m)
    => NodeResources ssc m -> Production ()
releaseNodeResources NodeResources {..} = do
    releaseAllHandlers
    whenJust nrJLogHandle (liftIO . hClose)
    closeNodeDBs nrDBs
    releaseNodeContext nrContext

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ssc m a.
      ( SscConstraint ssc
      , SecurityWorkersClass ssc
      , WithLogger m
      , MonadIO m
      , MonadMockable m
      )
    => NodeParams
    -> SscParams ssc
    -> (NodeResources ssc m -> Production a)
    -> Production a
bracketNodeResources np sp k = bracketTransport tcpAddr $ \transport ->
    maybeBracketKademlia np $ \mKademlia ->
        bracket (allocateNodeResources transport mKademlia np sp) releaseNodeResources k
  where
    tcpAddr = tpTcpAddr (npTransport np)

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    -- TODO: introduce Maybe FilePath builder for filePrefix
    let cfgBuilder = productionB <>
                     (mempty { _lcFilePrefix = lpHandlerPrefix })
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

allocateNodeContext
    :: forall ssc .
      (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeParams
    -> SscParams ssc
    -> ((Timestamp, TVar SlottingData) -> SlottingContextSum -> InitMode ssc ())
    -> InitMode ssc (NodeContext ssc)
allocateNodeContext np@NodeParams {..} sscnp putSlotting = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncBlkSemaphore <- BlkSemaphore <$> newEmptyMVar
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    ncSlottingVar <- (npSystemStart,) <$> mkSlottingVar
    ncSlottingContext <-
        case npUseNTP of
            True  -> SCNtp <$> mkNtpSlottingVar
            False -> SCSimple <$> mkSimpleSlottingVar
    putSlotting ncSlottingVar ncSlottingContext
    ncUserSecret <- newTVarIO $ npUserSecret
    ncBlockRetrievalQueue <- liftIO $ newTBQueueIO Const.blockRetrievalQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    ncProgressHeader <- liftIO newEmptyTMVarIO
    ncShutdownFlag <- newTVarIO False
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    ncUpdateContext <- mkUpdateContext
    ncSscContext <- untag @ssc sscCreateNodeContext sscnp
    ncSlogContext <- mkSlogContext
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
            , ncNetworkConfig = npNetworkConfig
#endif
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
       (MonadIO m, Mockable Catch m, Mockable Throw m, CanLog m)
    => BaseParams
    -> KademliaParams
    -> NodeType
    -> Bool
    -> m KademliaDHTInstance
createKademliaInstance BaseParams {..} kp peerType subscribe =
    usingLoggerName (lpRunnerTag bpLoggingParams) (startDHTInstance instConfig peerType subscribe)
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp ++ Const.defaultPeers}

-- | RAII for 'KademliaDHTInstance'.
bracketKademlia
    :: (MonadIO m, Mockable Catch m, Mockable Throw m, Mockable Bracket m, CanLog m)
    => BaseParams
    -> KademliaParams
    -> NodeType -- ^ Type to assign to Kademlia peers.
    -> Bool     -- ^ True if Kademlia peers should be known peers (MonadKnownPeers).
    -> (KademliaDHTInstance -> m a)
    -> m a
bracketKademlia bp kp peerType subscribe action =
    bracket (createKademliaInstance bp kp peerType subscribe) stopDHTInstance action

-- | The 'NodeParams' contain enough information to determine whether a Kademlia
-- instance should be brought up. Use this to safely acquire/release one.
maybeBracketKademlia
    :: (MonadIO m, Mockable Catch m, Mockable Throw m, Mockable Bracket m, CanLog m)
    => NodeParams
    -> (Maybe KademliaDHTInstance -> m a)
    -> m a
maybeBracketKademlia np action = mKp >>= \case
    Nothing -> action Nothing
    Just (kp, peerType, subscribe) -> bracketKademlia bp kp peerType subscribe (action . Just)
  where
    bp = npBaseParams np
    NetworkConfig {..} = npNetworkConfig np
    mKp = case (ncTopology, ncKademlia) of
        (TopologyP2P, Just kp) -> return $ Just (kp, NodeRelay, True)
        (TopologyP2P, Nothing) ->
            throw MissingKademliaParams
        (TopologyTraditional, Just kp) -> return $ Just (kp, NodeCore, True)
        (TopologyTraditional, Nothing) ->
            throw MissingKademliaParams
        (TopologyStatic NodeRelay _, Just kp) -> return $ Just (kp, NodeEdge, False)
        _ -> return $ Nothing

data MissingKademliaParams = MissingKademliaParams
    deriving (Show)

instance Exception MissingKademliaParams

----------------------------------------------------------------------------
-- Transport
----------------------------------------------------------------------------

createTransportTCP
    :: (MonadIO n, MonadIO m, WithLogger m, Mockable Throw m)
    => TCP.TCPAddr
    -> m (Transport n, m ())
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
        Right transport -> return (concrete transport, liftIO $ NT.closeTransport transport)

-- | RAII for 'Transport'.
bracketTransport
    :: ( MonadIO m, MonadIO n, Mockable Throw m, Mockable Bracket m, WithLogger m )
    => TCP.TCPAddr
    -> (Transport n -> m a)
    -> m a
bracketTransport tcpAddr k =
    bracket (createTransportTCP tcpAddr) snd (k . fst)
