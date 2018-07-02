{-# LANGUAGE CPP            #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}

-- | Resources used by node and ways to deal with them.

module Pos.Launcher.Resource
       (
         -- * Full resources
         NodeResources (..)

       --, allocateNodeResources
       --, releaseNodeResources
       , bracketNodeResources

         -- * Smaller resources
       , loggerBracket
       ) where

import           Universum

import           Control.Concurrent.STM (newEmptyTMVarIO, newTBQueueIO)
import           Control.Exception.Base (ErrorCall (..))
import           Data.Default (Default)
import qualified Data.Time as Time
import           Formatting (sformat, shown, (%))
import           Mockable (Production (..))
import           System.IO (BufferMode (..), hClose, hSetBuffering)
import qualified System.Metrics as Metrics

import           Network.Broadcast.OutboundQueue.Types (NodeType (..))
import           Pos.Binary ()
import           Pos.Block.Configuration (HasBlockConfiguration)
import           Pos.Block.Slog (mkSlogContext)
import           Pos.Client.CLI.Util (readLoggerConfig)
import           Pos.Configuration
import           Pos.Context (ConnectedPeers (..), NodeContext (..),
                     StartTime (..))
import           Pos.Core (HasConfiguration, Timestamp, gdStartTime,
                     genesisData)
import           Pos.DB (MonadDBRead, NodeDBs)
import           Pos.DB.Rocks (closeNodeDBs, openNodeDBs)
import           Pos.Delegation (DelegationVar, HasDlgConfiguration,
                     mkDelegationVar)
import qualified Pos.GState as GS
import           Pos.Infra.DHT.Real (KademliaParams (..))
import           Pos.Infra.Network.Types (NetworkConfig (..))
import           Pos.Infra.Reporting (initializeMisbehaviorMetrics)
import           Pos.Infra.Shutdown.Types (ShutdownContext (..))
import           Pos.Infra.Slotting (SimpleSlottingStateVar,
                     mkSimpleSlottingStateVar)
import           Pos.Infra.Slotting.Types (SlottingData)
import           Pos.Infra.StateLock (newStateLock)
import           Pos.Infra.Util.JsonLog.Events (JsonLogConfig (..),
                     jsonLogConfigFromHandle)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Pos.Lrc.Context (LrcContext (..), mkLrcSyncData)
import           Pos.Ssc (SscParams, SscState, createSscContext, mkSscState)
import           Pos.Txp (GenericTxpLocalData (..), TxpGlobalSettings,
                     mkTxpLocalData, recordTxpMetrics)

import           Pos.Launcher.Mode (InitMode, InitModeContext (..), runInitMode)
import           Pos.Update.Context (mkUpdateContext)
import qualified Pos.Update.DB as GState
import           Pos.Util (bracketWithTrace, newInitFuture)
import           Pos.Util.Log (LogContextT, LoggerConfig (..), LoggerName,
                     LoggingHandler (..), WithLogger, logDebug, logInfo)
import qualified Pos.Util.Log as Log (loggerBracket, parseLoggerConfig,
                     setupLogging)
import           Pos.Util.Trace (natTrace, noTrace)
import           Pos.Util.Trace.Named (TraceNamed)
import qualified Pos.Util.Trace.Named as TN (logDebug, logInfo, logWarning)

#ifdef linux_HOST_OS
import qualified System.Systemd.Daemon as Systemd
#endif


import qualified Pos.Util.Log.Internal as Internal

import           Control.Monad.Trans.Reader (mapReaderT)
import qualified Katip as K
import qualified Katip.Core as KC
import qualified Katip.Monadic as KM
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
    => TraceNamed Production
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> Production (NodeResources ext)
allocateNodeResources logTrace np@NodeParams {..} sscnp txpSettings initDB = do
    logInfo_ "Allocating node resources..."
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
        sscState <- mkSscState (natTrace (lift . ask) logTrace)  -- TODO  we need the real 'logTrace' here!
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
      logDebug_ msg = TN.logDebug logTrace msg
      logInfo_ msg = TN.logInfo logTrace msg

-- | Release all resources used by node. They must be released eventually.
releaseNodeResources ::
       NodeResources ext -> Production ()
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
    => TraceNamed Production
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> (HasConfiguration => NodeResources ext -> Production a)
    -> Production a
bracketNodeResources logTrace np sp txp initDB action = do
    let msg = "`NodeResources'"
    bracketWithTrace logTrace msg
            (allocateNodeResources logTrace np sp txp initDB)
            releaseNodeResources $ \nodeRes ->do
        -- Notify systemd we are fully operative
        -- FIXME this is not the place to notify.
        -- The network transport is not up yet.
        notifyReady logTrace
        action nodeRes

----------------------------------------------------------------------------
-- Logging
----------------------------------------------------------------------------

getRealLoggerConfig :: MonadIO m => LoggingParams -> m LoggerConfig
getRealLoggerConfig LoggingParams{..} =
    Log.parseLoggerConfig $ fromMaybe "--unk--" lpConfigPath
{-
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
-}

-- | RAII for Logging.   TODO  make use of Trace!!
loggerBracket :: (MonadIO m) => LoggingParams -> LogContextT Production a -> m a
--loggerBracket lp = bracket_ (setupLoggers lp) removeAllHandlers
loggerBracket params action = do
    lh <- liftIO $ Log.setupLogging =<< getRealLoggerConfig params
    liftIO $ Log.loggerBracket lh (lpDefaultName params) $
        (natLogContextT action)

-- natLogContextT :: K.KatipContextT Production a -> K.KatipContextT IO a
-- natLogContextT (KM.KatipContextT p) = KM.KatipContextT $ mapReaderT runProduction p

demo_action :: Int -> Production ()
demo_action _ = return ()

-- loggerBracket' :: LoggingHandler -> LoggerName -> (LogContextT Production) a -> Production a
-- loggerBracket' lh name f = do
--     mayle <- liftIO $ Internal.getLogEnv lh
--     case mayle of
--             Nothing -> error "logging not yet initialized. Abort."
--             Just le -> liftIO $ bracket (return le) K.closeScribes $
--                 \le_ -> runProduction $ K.runKatipContextT le_ () (Internal.s2kname name) $ f


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
    :: forall ext m .
      ( HasConfiguration
      , HasNodeConfiguration
      , HasBlockConfiguration
      )
    => TraceNamed Production
    -> AllocateNodeContextData ext
    -> TxpGlobalSettings
    -> Metrics.Store
    -> InitMode NodeContext
allocateNodeContext logTrace ancd txpSettings ekgStore = do
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
    ncStateLockMetrics <- liftIO $ recordTxpMetrics noTrace store txpMemPool
    logDebug_ "Created StateLock metrics"
    lcLrcSync <- mkLrcSyncData >>= newTVarIO
    logDebug_ "Created LRC sync"
    ncSlottingVar <- (gdStartTime genesisData,) <$> mkSlottingVar
    logDebug_ "Created slotting variable"
    ncSlottingContext <- mkSimpleSlottingStateVar
    logDebug_ "Created slotting context"
    putSlotting ncSlottingVar ncSlottingContext
    logDebug_ "Filled slotting future"
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
        logInfo_ m = lift $ TN.logInfo logTrace m
        logDebug_ m = lift $ TN.logDebug logTrace m

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
        Nothing -> TN.logWarning logTrace "notifyReady failed to notify systemd."
#else
notifyReady :: MonadIO m => TraceNamed m -> m ()
notifyReady logTrace = TN.logInfo logTrace "notifyReady: no systemd support enabled"
#endif
