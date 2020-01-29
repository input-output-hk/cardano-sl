{-# LANGUAGE CPP             #-}
{-# LANGUAGE KindSignatures  #-}
{-# LANGUAGE Rank2Types      #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators   #-}

-- | Resources used by node and ways to deal with them.

module Pos.Launcher.Resource
       (
         -- * Full resources
         NodeResources (..)

       , bracketNodeResources

         -- * Smaller resources
       , loggerBracket
       ) where

import           Universum

import           Control.Concurrent.Async.Lifted (Async)
import qualified Control.Concurrent.Async.Lifted as Async
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
import           Pos.Chain.Genesis as Genesis (Config, configEpochSlots,
                     configStartTime)
import           Pos.Chain.Ssc (SscParams, SscState, createSscContext)
import           Pos.Configuration
import           Pos.Context (ConnectedPeers (..), NodeContext (..),
                     StartTime (..))
import           Pos.Core (Timestamp)
import           Pos.Core.Reporting (initializeMisbehaviorMetrics)
import           Pos.DB (MonadDBRead, NodeDBs)
import           Pos.DB.Block (consolidateWorker, mkSlogContext)
import           Pos.DB.Delegation (mkDelegationVar)
import           Pos.DB.Lrc (LrcContext (..), mkLrcSyncData)
import           Pos.DB.Rocks (closeNodeDBs, openNodeDBs)
import           Pos.DB.Ssc (mkSscState)
import           Pos.DB.Txp (GenericTxpLocalData (..), TxpGlobalSettings,
                     mkTxpLocalData, recordTxpMetrics)
import           Pos.DB.Update (mkUpdateContext)
import qualified Pos.DB.Update as GState
import qualified Pos.GState as GS
import           Pos.Infra.InjectFail (FInjects)
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Shutdown.Types (ShutdownContext (..))
import           Pos.Infra.Slotting (SimpleSlottingStateVar,
                     mkSimpleSlottingStateVar)
import           Pos.Infra.Slotting.Types (SlottingData)
import           Pos.Infra.StateLock (newStateLock)
import           Pos.Infra.Util.JsonLog.Events (JsonLogConfig (..),
                     jsonLogConfigFromHandle)
import           Pos.Launcher.Mode (InitMode, InitModeContext (..), runInitMode)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Pos.Util (bracketWithLogging, newInitFuture)
import           Pos.Util.Log.Internal (LoggingHandler)
import           Pos.Util.Log.LoggerConfig (defaultInteractiveConfiguration,
                     isWritingToConsole, lcLoggerTree, ltHandlers)
import           Pos.Util.Wlog (LoggerConfig (..), Severity (..), WithLogger,
                     logDebug, logInfo, parseLoggerConfig, removeAllHandlers,
                     setupLogging')

#ifdef linux_HOST_OS
import qualified Pos.Util.Wlog as Logger
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
    , nrConsolidate   :: !(Async ())
    , nrFInjects      :: !(FInjects IO)
    }

----------------------------------------------------------------------------
-- Allocation/release/bracket
----------------------------------------------------------------------------

-- | Allocate all resources used by node. They must be released eventually.
allocateNodeResources
    :: forall ext .
       ( Default ext
       , HasNodeConfiguration
       , HasDlgConfiguration
       , HasBlockConfiguration
       )
    => Genesis.Config
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> IO (NodeResources ext)
allocateNodeResources genesisConfig np@NodeParams {..} sscnp txpSettings initDB = do
    logInfo "Allocating node resources..."
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
    logDebug "Opened DB, created some futures, going to run InitMode"
    runInitMode initModeContext $ do
        initDB
        logDebug "Initialized DB"

        consAsync <- Async.async $ consolidateWorker genesisConfig
        logDebug "Initialized block/epoch consolidation"

        nrEkgStore <- liftIO $ Metrics.newStore
        logDebug "Created EKG store"

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
        ctx@NodeContext {..} <-
            allocateNodeContext genesisConfig ancd txpSettings nrEkgStore
        putLrcContext ncLrcContext
        logDebug "Filled LRC Context future"
        dlgVar <- mkDelegationVar
        logDebug "Created DLG var"
        sscState <- mkSscState $ configEpochSlots genesisConfig
        logDebug "Created SSC var"
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
        logDebug "JSON configuration initialized"

        logDebug "Finished allocating node resources!"
        return NodeResources
            { nrContext = ctx
            , nrDBs = db
            , nrSscState = sscState
            , nrTxpState = txpVar
            , nrDlgState = dlgVar
            , nrJsonLogConfig = jsonLogConfig
            , nrConsolidate = consAsync
            , nrFInjects = npFInjects
            , ..
            }

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
    Async.cancel nrConsolidate
    releaseNodeContext nrContext

-- | Run computation which requires 'NodeResources' ensuring that
-- resources will be released eventually.
bracketNodeResources :: forall ext a.
      ( Default ext
      , HasNodeConfiguration
      , HasDlgConfiguration
      , HasBlockConfiguration
      )
    => Genesis.Config
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> (NodeResources ext -> IO a)
    -> IO a
bracketNodeResources genesisConfig np sp txp initDB action = do
    let msg = "`NodeResources'"
    bracketWithLogging msg
            (allocateNodeResources genesisConfig np sp txp initDB)
            releaseNodeResources $ \nodeRes ->do
        -- Notify systemd we are fully operative
        -- FIXME this is not the place to notify.
        -- The network transport is not up yet.
        notifyReady
        action nodeRes

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} = do
    case lpConfigPath of
        Just configPath -> parseLoggerConfig configPath
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
        Just True  -> (<> defaultInteractiveConfiguration Info)
                      -- add output to the console with severity filter >= Info
        Just False -> lcLoggerTree . ltHandlers %~ filter (not . isWritingToConsole)

setupLoggers :: MonadIO m => Text -> LoggingParams -> m LoggingHandler
setupLoggers cfoKey params = setupLogging' cfoKey =<< getRealLoggerConfig params

-- | RAII for Logging.
loggerBracket :: Text -> LoggingParams -> IO a -> IO a
loggerBracket cfoKey lp action = bracket (setupLoggers cfoKey lp) removeAllHandlers (const action)

----------------------------------------------------------------------------
-- NodeContext
----------------------------------------------------------------------------

data AllocateNodeContextData ext = AllocateNodeContextData
    { ancdNodeParams  :: !NodeParams
    , ancdSscParams   :: !SscParams
    , ancdPutSlotting :: (Timestamp, TVar SlottingData) -> SimpleSlottingStateVar -> InitMode ()
    , ancdNetworkCfg  :: NetworkConfig ()
    , ancdEkgStore    :: !Metrics.Store
    , ancdTxpMemState :: !(GenericTxpLocalData ext)
    }

allocateNodeContext
    :: forall ext .
      (HasNodeConfiguration, HasBlockConfiguration)
    => Genesis.Config
    -> AllocateNodeContextData ext
    -> TxpGlobalSettings
    -> Metrics.Store
    -> InitMode NodeContext
allocateNodeContext genesisConfig ancd txpSettings ekgStore = do
    let epochSlots = configEpochSlots genesisConfig
    let AllocateNodeContextData { ancdNodeParams = np@NodeParams {..}
                                , ancdSscParams = sscnp
                                , ancdPutSlotting = putSlotting
                                , ancdNetworkCfg = networkConfig
                                , ancdEkgStore = store
                                , ancdTxpMemState = TxpLocalData {..}
                                } = ancd
    logInfo "Allocating node context..."
    ncLoggerConfig <- getRealLoggerConfig $ bpLoggingParams npBaseParams
    logDebug "Got logger config"
    ncStateLock <- newStateLock =<< GS.getTip
    logDebug "Created a StateLock"
    ncStateLockMetrics <- liftIO $ recordTxpMetrics store txpMemPool
    logDebug "Created StateLock metrics"
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    logDebug "Created LRC sync"
    ncSlottingVar <- (configStartTime genesisConfig,) <$> mkSlottingVar
    logDebug "Created slotting variable"
    ncSlottingContext <- mkSimpleSlottingStateVar epochSlots
    logDebug "Created slotting context"
    putSlotting ncSlottingVar ncSlottingContext
    logDebug "Filled slotting future"
    ncUserSecret <- newTVarIO $ npUserSecret
    logDebug "Created UserSecret variable"
    ncBlockRetrievalQueue <- liftIO $ newTBQueueIO blockRetrievalQueueSize
    ncRecoveryHeader <- liftIO newEmptyTMVarIO
    logDebug "Created block retrieval queue, recovery and progress headers"
    ncShutdownFlag <- newTVarIO Nothing
    ncStartTime <- StartTime <$> liftIO Time.getCurrentTime
    ncLastKnownHeader <- newTVarIO Nothing
    logDebug "Created last known header and shutdown flag variables"
    ncUpdateContext <- mkUpdateContext epochSlots
    logDebug "Created context for update"
    ncSscContext <- createSscContext sscnp
    logDebug "Created context for ssc"
    ncSlogContext <- mkSlogContext genesisConfig store
    logDebug "Created context for slog"
    -- TODO synchronize the NodeContext peers var with whatever system
    -- populates it.
    peersVar <- newTVarIO mempty
    logDebug "Created peersVar"
    mm <- initializeMisbehaviorMetrics ekgStore

    logDebug ("Dequeue policy to core:  " <> (show ((ncDequeuePolicy networkConfig) NodeCore)))
        `catch` \(ErrorCall msg) -> logDebug (toText msg)
    logDebug ("Dequeue policy to relay: " <> (show ((ncDequeuePolicy networkConfig) NodeRelay)))
        `catch` \(ErrorCall msg) -> logDebug (toText msg)
    logDebug ("Dequeue policy to edge: " <> (show ((ncDequeuePolicy networkConfig) NodeEdge)))
        `catch` \(ErrorCall msg) -> logDebug (toText msg)


    logDebug "Finished allocating node context!"
    let ctx =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
            , ncLrcContext = LrcContext {..}
            , ncShutdownContext = ShutdownContext ncShutdownFlag npFInjects
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

-- | Notify process manager tools like systemd the node is ready.
-- Available only on Linux for systems where `libsystemd-dev` is installed.
-- It defaults to a noop for all the other platforms.
#ifdef linux_HOST_OS
notifyReady :: (MonadIO m, WithLogger m) => m ()
notifyReady = do
    res <- liftIO Systemd.notifyReady
    case res of
        Just () -> return ()
        Nothing -> Logger.logWarning "notifyReady failed to notify systemd."
#else
notifyReady :: (WithLogger m) => m ()
notifyReady = logInfo "notifyReady: no systemd support enabled"
#endif
