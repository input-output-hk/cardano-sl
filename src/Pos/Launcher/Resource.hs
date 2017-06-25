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

import           Universum                   hiding (bracket, finally)

import           Control.Concurrent.STM      (newEmptyTMVarIO, newTBQueueIO)
import           Data.Default                (def)
import           Data.Tagged                 (untag)
import qualified Data.Time                   as Time
import           Formatting                  (sformat, shown, (%))
import           Mockable                    (Catch, Mockable, Production (..), Throw,
                                              bracket, throw)
import           Network.QDisc.Fair          (fairQDisc)
import           Network.Transport.Abstract  (Transport, closeTransport, hoistTransport)
import           Network.Transport.Concrete  (concrete)
import qualified Network.Transport.TCP       as TCP
import qualified STMContainers.Map           as SM
import           System.IO                   (Handle, hClose)
import           System.Wlog                 (CanLog, LoggerConfig (..), WithLogger,
                                              getLoggerName, logError, productionB,
                                              releaseAllHandlers, setupLogging,
                                              usingLoggerName)

import           Pos.Binary                  ()
import           Pos.CLI                     (readLoggerConfig)
import           Pos.Communication.PeerState (PeerStateCtx)
import qualified Pos.Constants               as Const
import           Pos.Context                 (BlkSemaphore (..), ConnectedPeers (..),
                                              GenesisStakes (..), GenesisUtxo (..),
                                              NodeContext (..), StartTime (..))
import           Pos.Core                    (Timestamp)
import           Pos.DB                      (MonadDBRead, NodeDBs)
import           Pos.DB.DB                   (closeNodeDBs, initNodeDBs, openNodeDBs)
import           Pos.DB.GState               (getTip)
import           Pos.Delegation.Class        (DelegationVar)
import           Pos.DHT.Real                (KademliaDHTInstance, KademliaParams (..),
                                              startDHTInstance, stopDHTInstance)
import           Pos.Discovery               (DiscoveryContextSum (..))
import           Pos.Launcher.Param          (BaseParams (..), LoggingParams (..),
                                              NetworkParams (..), NodeParams (..))
import           Pos.Lrc.Context             (LrcContext (..), mkLrcSyncData)
import           Pos.Shutdown.Types          (ShutdownContext (..))
import           Pos.Slotting                (SlottingContextSum (..), SlottingVar,
                                              mkNtpSlottingVar)
import           Pos.Ssc.Class               (SscConstraint, SscParams,
                                              sscCreateNodeContext)
import           Pos.Ssc.Extra               (SscState, mkSscState)
import           Pos.Txp                     (GenericTxpLocalData, mkTxpLocalData)
#ifdef WITH_EXPLORER
import           Pos.Explorer                (explorerTxpGlobalSettings)
#else
import           Pos.Txp                     (txpGlobalSettings)
#endif
import           Pos.Launcher.Mode           (InitMode, InitModeContext (..),
                                              newInitFuture, runInitMode)
import           Pos.Security                (SecurityWorkersClass)
import           Pos.Update.Context          (mkUpdateContext)
import qualified Pos.Update.DB               as GState
import           Pos.Util.Concurrent.RWVar   as RWV
import           Pos.Util.Util               (powerLift)
import           Pos.Worker                  (allWorkersCount)
import           Pos.WorkMode                (TxpExtra_TMP)

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
    , nrPeerState  :: !(PeerStateCtx Production)
    , nrTransport  :: !(Transport m)
    , nrJLogHandle :: !(Maybe Handle)
    -- ^ Handle for JSON logging (optional).
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
    :: forall ssc.
      (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeParams
    -> SscParams ssc
    -> Production (NodeResources ssc Production)
allocateNodeResources np@NodeParams {..} sscnp = do
    db <- openNodeDBs npRebuildDb npDbPathM
    (futureLrcContext, putLrcContext) <- newInitFuture
    (futureSlottingVar, putSlottingVar) <- newInitFuture
    (futureSlottingContext, putSlottingContext) <- newInitFuture
    let putSlotting sv sc = do
            putSlottingVar sv
            putSlottingContext sc
        initModeContext = InitModeContext
            db
            (GenesisUtxo npCustomUtxo)
            (GenesisStakes npGenesisStakes)
            futureSlottingVar
            futureSlottingContext
            futureLrcContext
    runInitMode initModeContext $ do
        initNodeDBs @ssc npSystemStart
        ctx@NodeContext {..} <- allocateNodeContext np sscnp putSlotting
        putLrcContext ncLrcContext
        initTip <- getTip
        setupLoggers $ bpLoggingParams npBaseParams
        dlgVar <- RWV.new def
        txpVar <- mkTxpLocalData mempty initTip
        peerState <- liftIO SM.newIO
        sscState <- mkSscState @ssc
        nrTransport <- powerLift @Production $ createTransportTCP $ npTcpAddr npNetwork
        nrJLogHandle <-
            case npJLFile of
                Nothing -> pure Nothing
                Just fp -> Just <$> openFile fp WriteMode
        return NodeResources
            { nrContext = ctx
            , nrDBs = db
            , nrSscState = sscState
            , nrTxpState = txpVar
            , nrDlgState = dlgVar
            , nrPeerState = peerState
            , ..
            }

-- | Release all resources used by node. They must be released eventually.
releaseNodeResources ::
       forall ssc m. (SscConstraint ssc, MonadIO m)
    => NodeResources ssc m -> m ()
releaseNodeResources NodeResources {..} = do
    releaseAllHandlers
    whenJust nrJLogHandle (liftIO . hClose)
    closeNodeDBs nrDBs
    releaseNodeContext nrContext
    closeTransport nrTransport

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ssc a.
      (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeParams
    -> SscParams ssc
    -> (NodeResources ssc Production -> Production a)
    -> Production a
bracketNodeResources np sp =
    bracket (allocateNodeResources np sp) releaseNodeResources

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
    -> (SlottingVar -> SlottingContextSum -> InitMode ssc ())
    -> InitMode ssc (NodeContext ssc)
allocateNodeContext np@NodeParams {..} sscnp putSlotting = do
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    ncBlkSemaphore <- BlkSemaphore <$> newEmptyMVar
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    ncDiscoveryContext <-
        case npDiscovery npNetwork of
            Left peers -> pure (DCStatic peers)
            Right kadParams ->
                DCKademlia <$> createKademliaInstance npBaseParams kadParams
    ncSlottingVar <- mkSlottingVar npSystemStart
    ncSlottingContext <-
        case npUseNTP of
            True  -> SCNtp <$> mkNtpSlottingVar
            False -> pure SCSimple
    putSlotting ncSlottingVar ncSlottingContext
    ncUserSecret <- newTVarIO $ npUserSecret
    ncBlockRetrievalQueue <- liftIO $ newTBQueueIO Const.blockRetrievalQueueSize
    ncInvPropagationQueue <- liftIO $ newTBQueueIO Const.propagationQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    ncProgressHeader <- liftIO newEmptyTMVarIO
    ncShutdownFlag <- newTVarIO False
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    ncUpdateContext <- mkUpdateContext
    ncSscContext <- untag @ssc sscCreateNodeContext sscnp
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
            , ..
            }
    -- This queue won't be used.
    fakeQueue <- liftIO (newTBQueueIO 100500)
    let allWorkersNum = allWorkersCount @ssc (ctx fakeQueue)
    ctx <$> liftIO (newTBQueueIO allWorkersNum)

releaseNodeContext :: forall ssc m . MonadIO m => NodeContext ssc -> m ()
releaseNodeContext NodeContext {..} =
    case ncDiscoveryContext of
        DCKademlia kademlia -> stopDHTInstance kademlia
        DCStatic _          -> pass

-- Create new 'SlottingVar' using data from DB. Probably it would be
-- good to have it in 'infra', but it's complicated.
mkSlottingVar :: (MonadIO m, MonadDBRead m) => Timestamp -> m SlottingVar
mkSlottingVar sysStart = do
    sd <- GState.getSlottingData
    (sysStart, ) <$> newTVarIO sd

----------------------------------------------------------------------------
-- Kademlia
----------------------------------------------------------------------------

createKademliaInstance ::
       (MonadIO m, Mockable Catch m, Mockable Throw m, CanLog m)
    => BaseParams
    -> KademliaParams
    -> m KademliaDHTInstance
createKademliaInstance BaseParams {..} kp =
    usingLoggerName (lpRunnerTag bpLoggingParams) (startDHTInstance instConfig)
  where
    instConfig = kp {kpPeers = ordNub $ kpPeers kp ++ Const.defaultPeers}

-- | RAII for 'KademliaDHTInstance'.
bracketKademlia
    :: BaseParams
    -> KademliaParams
    -> (KademliaDHTInstance -> Production a)
    -> Production a
bracketKademlia bp kp action =
    bracket (createKademliaInstance bp kp) stopDHTInstance action

----------------------------------------------------------------------------
-- Transport
----------------------------------------------------------------------------

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

-- | RAII for 'Transport'.
bracketTransport
    :: TCP.TCPAddr
    -> (Transport Production -> Production a)
    -> Production a
bracketTransport tcpAddr =
    bracket (createTransportTCP tcpAddr) (closeTransport)
