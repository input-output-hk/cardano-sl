{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

-- | Resources used by node and ways to deal with them.

module Pos.Launcher.Resource
       (
         -- * Full resources
         NodeResources (..)

       , bracketNodeResources

         -- * Smaller resources
       , getRealLoggerConfig
       ) where

import           Universum

import           Control.Concurrent.STM (newEmptyTMVarIO, newTBQueueIO)
import           Control.Exception.Base (ErrorCall (..))
import           Data.Default (Default)
import qualified Data.Time as Time
import           Formatting (sformat, shown, (%))
import           System.IO (BufferMode (..), hClose, hSetBuffering)
import qualified System.Metrics as Metrics

import           Network.Broadcast.OutboundQueue.Types (NodeType (..))
import           Pos.Binary ()
import           Pos.Chain.Block (HasBlockConfiguration)
import           Pos.Chain.Delegation (DelegationVar, HasDlgConfiguration)
import           Pos.Chain.Ssc (SscParams, SscState, createSscContext)
import           Pos.Configuration
import           Pos.Context (ConnectedPeers (..), NodeContext (..),
                     StartTime (..))
import           Pos.Core (HasConfiguration, Timestamp, genesisData)
import           Pos.Core.Genesis (gdStartTime)
import           Pos.Core.Reporting (initializeMisbehaviorMetrics)
import           Pos.DB (MonadDBRead, NodeDBs)
import           Pos.DB.Block (mkSlogContext)
import           Pos.DB.Delegation (mkDelegationVar)
import           Pos.DB.GState.Lock (newStateLock)
import           Pos.DB.Lrc (LrcContext (..), mkLrcSyncData)
import           Pos.DB.Rocks (closeNodeDBs, openNodeDBs)
import           Pos.DB.Ssc (mkSscState)
import           Pos.DB.Txp (GenericTxpLocalData (..), TxpGlobalSettings,
                     mkTxpLocalData, recordTxpMetrics)
import           Pos.DB.Update (mkUpdateContext)
import qualified Pos.DB.Update as GState
import qualified Pos.GState as GS
import           Pos.Infra.DHT.Real (KademliaParams (..))
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Shutdown.Types (ShutdownContext (..))
import           Pos.Infra.Slotting (SimpleSlottingStateVar,
                     mkSimpleSlottingStateVar)
import           Pos.Infra.Slotting.Types (SlottingData)
import           Pos.Infra.Util.JsonLog.Events (JsonLogConfig (..),
                     jsonLogConfigFromHandle)
import           Pos.Launcher.Mode (InitMode, InitModeContext (..), runInitMode)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Pos.Util (bracketWithTrace, newInitFuture)
import qualified Pos.Util.Log as Log
import           Pos.Util.LoggerConfig (LoggerConfig (..),
                     defaultInteractiveConfiguration)
#ifdef linux_HOST_OS
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     logInfo, logWarning, natTrace)
#else
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     logInfo, natTrace)
#endif

#ifdef linux_HOST_OS
import qualified System.Systemd.Daemon as Systemd
#endif

----------------------------------------------------------------------------
-- Data type
----------------------------------------------------------------------------

-- | This data type contains all resources used by node.
data NodeResources ext = NodeResources
    { nrContext       :: !NodeContext
    , nrDBs           :: !NodeDBs
    , nrSscState      :: !SscState
    , nrTxpState      :: !(GenericTxpLocalData ext)
    , nrDlgState      :: !DelegationVar
    , nrJsonLogConfig :: !JsonLogConfig
    -- ^ Config for optional JSON logging.
    , nrEkgStore      :: !Metrics.Store
    }

----------------------------------------------------------------------------
-- Allocation/release/bracket
----------------------------------------------------------------------------

-- | Allocate all resources used by node. They must be released eventually.
allocateNodeResources
    :: forall ext .
       ( Default ext
       , HasConfiguration
       , HasNodeConfiguration
       , HasDlgConfiguration
       , HasBlockConfiguration
       )
    => TraceNamed IO
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> IO (NodeResources ext)
allocateNodeResources logTrace0 np@NodeParams {..} sscnp txpSettings initDB = do
    logInfo logTrace "Allocating node resources..."
    npDbPath <- case npDbPathM of
        Nothing -> do
            let dbPath = "node-db" :: FilePath
            logInfo_ $ sformat ("DB path not specified, defaulting to "%
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
    logDebug_ "Opened DB, created some futures, going to run InitMode"
    runInitMode initModeContext $ do
        initDB
        lift $ logDebug_ "Initialized DB"

        nrEkgStore <- liftIO $ Metrics.newStore
        lift $ logDebug_ "Created EKG store"

        txpVar <- mkTxpLocalData -- doesn't use slotting or LRC
        let ancd =
                AllocateNodeContextData
                { ancdNodeParams = np
                , ancdSscParams = sscnp
                , ancdPutSlotting = putSlotting
                , ancdNetworkCfg = npNetworkConfig
                , ancdEkgStore = nrEkgStore
                , ancdTxpMemState = txpVar
                }
        ctx@NodeContext {..} <- allocateNodeContext logTrace ancd txpSettings nrEkgStore
        putLrcContext ncLrcContext
        lift $ logDebug_ "Filled LRC Context future"
        dlgVar <- mkDelegationVar
        lift $ logDebug_ "Created DLG var"
        sscState <- mkSscState (natTrace (lift . ask) logTraceP)  -- TODO  we need the real 'logTrace' here!
        lift $ logDebug_ "Created SSC var"
        jsonLogHandle <-
            case npJLFile of
                Nothing -> pure Nothing
                Just fp -> do
                    h <- openFile fp WriteMode
                    liftIO $ hSetBuffering h LineBuffering
                    return $ Just h
        jsonLogConfig <- maybe
            (pure JsonLogDisabled)
            jsonLogConfigFromHandle
            jsonLogHandle
        lift $ logDebug_ "JSON configuration initialized"

        lift $ logDebug_ "Finished allocating node resources!"
        return NodeResources
            { nrContext = ctx
            , nrDBs = db
            , nrSscState = sscState
            , nrTxpState = txpVar
            , nrDlgState = dlgVar
            , nrJsonLogConfig = jsonLogConfig
            , ..
            }
    where
      logTrace = appendName "allocateNodeResources" logTrace0
      logTraceP = natTrace liftIO logTrace
      logDebug_ msg = logDebug logTraceP msg
      logInfo_ msg = logInfo logTraceP msg

-- | Release all resources used by node. They must be released eventually.
releaseNodeResources ::
       NodeResources ext -> IO ()
releaseNodeResources NodeResources {..} = do
    case nrJsonLogConfig of
        JsonLogDisabled -> return ()
        JsonLogConfig mVarHandle _ -> do
            h <- takeMVar mVarHandle
            (liftIO . hClose) h
            putMVar mVarHandle h
    closeNodeDBs nrDBs
    releaseNodeContext nrContext

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ext a.
      ( Default ext
      , HasConfiguration
      , HasNodeConfiguration
      , HasDlgConfiguration
      , HasBlockConfiguration
      )
    => TraceNamed IO
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> (HasConfiguration => NodeResources ext -> IO a)
    -> IO a
bracketNodeResources logTrace np sp txp initDB action = do
    let msg = "`NodeResources'"
    let logTraceP = natTrace liftIO logTrace
    bracketWithTrace logTraceP msg
            (allocateNodeResources logTrace np sp txp initDB)
            releaseNodeResources $ \nodeRes -> do
        -- Notify systemd we are fully operative
        -- FIXME this is not the place to notify.
        -- The network transport is not up yet.
        notifyReady logTraceP
        action nodeRes

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: MonadIO m => LoggingParams -> m Log.LoggerConfig
getRealLoggerConfig LoggingParams{..} =
    case lpConfigPath of
        Just configPath -> Log.parseLoggerConfig configPath
        Nothing         -> return (mempty :: LoggerConfig)
    >>= \lc -> return $ (overridePrefixPath . overrideConsoleLog) lc
  where
    overridePrefixPath :: LoggerConfig -> LoggerConfig
    overridePrefixPath = case lpHandlerPrefix of
        Nothing -> identity
        Just _  -> \lc -> lc { _lcBasePath = lpHandlerPrefix }
    overrideConsoleLog :: LoggerConfig -> LoggerConfig
    overrideConsoleLog = case lpConsoleLog of
        Nothing    -> identity
        Just True  -> (<>) (defaultInteractiveConfiguration Log.Info)
        Just False -> identity

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data AllocateNodeContextData ext = AllocateNodeContextData
    { ancdNodeParams  :: !NodeParams
    , ancdSscParams   :: !SscParams
    , ancdPutSlotting :: (Timestamp, TVar SlottingData) -> SimpleSlottingStateVar -> InitMode ()
    , ancdNetworkCfg  :: NetworkConfig KademliaParams
    , ancdEkgStore    :: !Metrics.Store
    , ancdTxpMemState :: !(GenericTxpLocalData ext)
    }

allocateNodeContext
    :: forall ext .
      ( HasConfiguration
      , HasNodeConfiguration
      , HasBlockConfiguration
      )
    => TraceNamed IO
    -> AllocateNodeContextData ext
    -> TxpGlobalSettings
    -> Metrics.Store
    -> InitMode NodeContext
allocateNodeContext logTrace0 ancd txpSettings ekgStore = do
    let AllocateNodeContextData { ancdNodeParams = np@NodeParams {..}
                                , ancdSscParams = sscnp
                                , ancdPutSlotting = putSlotting
                                , ancdNetworkCfg = networkConfig
                                , ancdEkgStore = store
                                , ancdTxpMemState = TxpLocalData {..}
                                } = ancd
    logInfo_ "Allocating node context..."
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    logDebug_ "Got logger config"
    ncStateLock <- newStateLock =<< GS.getTip
    logDebug_ "Created a StateLock"
    ncStateLockMetrics <- liftIO $ recordTxpMetrics logTrace store txpMemPool
    logDebug_ "Created StateLock metrics"
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    logDebug_ "Created LRC sync"
    ncSlottingVar <- (gdStartTime genesisData,) <$> mkSlottingVar
    logDebug_ "Created slotting variable"
    ncSlottingContext <- mkSimpleSlottingStateVar
    logDebug_ "Created slotting context"
    putSlotting ncSlottingVar ncSlottingContext
    logDebug_ "Filled slotting future"
    ncUserPublic <- newTVarIO $ npUserPublic
    logDebug_ "Created UserPublic variable"
    ncUserSecret <- newTVarIO $ npUserSecret
    logDebug_ "Created UserSecret variable"
    ncBlockRetrievalQueue <- liftIO $ newTBQueueIO blockRetrievalQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    logDebug_ "Created block retrieval queue, recovery and progress headers"
    ncShutdownFlag <- newTVarIO False
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    logDebug_ "Created last known header and shutdown flag variables"
    ncUpdateContext <- mkUpdateContext
    logDebug_ "Created context for update"
    ncSscContext <- createSscContext sscnp
    logDebug_ "Created context for ssc"
    ncSlogContext <- mkSlogContext store
    logDebug_ "Created context for slog"
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    logDebug_ "Created peersVar"
    mm <- initializeMisbehaviorMetrics ekgStore

    logDebug_ ("Dequeue policy to core:  " <> (show ((ncDequeuePolicy networkConfig) NodeCore)))
        `catch` \(ErrorCall msg) -> logDebug_ (toText msg)
    logDebug_ ("Dequeue policy to relay: " <> (show ((ncDequeuePolicy networkConfig) NodeRelay)))
        `catch` \(ErrorCall msg) -> logDebug_ (toText msg)
    logDebug_ ("Dequeue policy to edge: " <> (show ((ncDequeuePolicy networkConfig) NodeEdge)))
        `catch` \(ErrorCall msg) -> logDebug_ (toText msg)


    logDebug_ "Finished allocating node context!"
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
      where
        logTrace = appendName "allocateNodeContext" logTrace0
        logTraceP = natTrace liftIO logTrace
        logInfo_ m = lift $ logInfo logTraceP m
        logDebug_ m = lift $ logDebug logTraceP m

releaseNodeContext :: forall m . MonadIO m => NodeContext -> m ()
releaseNodeContext _ = return ()

-- Create new 'SlottingVar' using data from DB. Probably it would be
-- good to have it in 'infra', but it's complicated.
mkSlottingVar :: (MonadIO m, MonadDBRead m) => m (TVar SlottingData)
mkSlottingVar = newTVarIO =<< GState.getSlottingData

-- | Notify process manager tools like systemd the node is ready.
-- Available only on Linux for systems where `libsystemd-dev` is installed.
-- It defaults to a noop for all the other platforms.
#ifdef linux_HOST_OS
notifyReady :: MonadIO m => TraceNamed m -> m ()
notifyReady logTrace = do
    res <- liftIO Systemd.notifyReady
    case res of
        Just () -> return ()
        Nothing -> logWarning logTrace "notifyReady failed to notify systemd."
#else
notifyReady :: TraceNamed m -> m ()
notifyReady logTrace = logInfo logTrace "notifyReady: no systemd support enabled"
#endif
