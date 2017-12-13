{-# LANGUAGE CPP            #-}
{-# LANGUAGE Rank2Types     #-}
{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE KindSignatures #-}

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
       ) where

import           Universum hiding (bracket)

import           Control.Concurrent.STM (newEmptyTMVarIO, newTBQueueIO)
import           Data.Default (Default)
import qualified Data.Time as Time
import           Formatting (sformat, shown, (%))
import           Mockable (Production (..), bracket)
import           System.IO (BufferMode (..), Handle, hClose, hSetBuffering)
import qualified System.Metrics as Metrics
import           System.Wlog (LoggerConfig (..), WithLogger, consoleActionB,
                              defaultHandleAction, logInfo, maybeLogsDirB, productionB,
                              removeAllHandlers, setupLogging, showTidB)

import           Pos.Binary ()
import           Pos.Block.Slog (mkSlogContext)
import           Pos.Client.CLI.Util (readLoggerConfig)
import           Pos.Configuration
import           Pos.Context (ConnectedPeers (..), NodeContext (..), StartTime (..))
import           Pos.Core (HasConfiguration, Timestamp, gdStartTime, genesisData)
import           Pos.DB (MonadDBRead, NodeDBs)
import           Pos.DB.Rocks (closeNodeDBs, openNodeDBs)
import           Pos.Delegation (DelegationVar, HasDlgConfiguration, mkDelegationVar)
import           Pos.DHT.Real (KademliaParams (..))
import qualified Pos.GState as GS
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Lrc.Context (LrcContext (..), mkLrcSyncData)
import           Pos.Network.Types (NetworkConfig (..))
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
-- FIXME remove 'm' parameter, as it is not used.
data NodeResources ext (m :: * -> *) = NodeResources
    { nrContext    :: !NodeContext
    , nrDBs        :: !NodeDBs
    , nrSscState   :: !SscState
    , nrTxpState   :: !(GenericTxpLocalData ext)
    , nrDlgState   :: !DelegationVar
    , nrJLogHandle :: !(Maybe Handle)
    -- ^ Handle for JSON logging (optional).
    , nrEkgStore   :: !Metrics.Store
    }

hoistNodeResources
    :: forall ext n m .
       (forall a. n a -> m a)
    -> NodeResources ext n
    -> NodeResources ext m
hoistNodeResources _ NodeResources {..} = NodeResources {..}

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
    => NetworkConfig KademliaParams
    -> NodeParams
    -> SscParams
    -> TxpGlobalSettings
    -> InitMode ()
    -> Production (NodeResources ext m)
allocateNodeResources networkConfig np@NodeParams {..} sscnp txpSettings initDB = do
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
        ctx@NodeContext {..} <- allocateNodeContext ancd txpSettings
        putLrcContext ncLrcContext
        dlgVar <- mkDelegationVar
        sscState <- mkSscState
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
    bracket (allocateNodeResources (npNetworkConfig np) np sp txp initDB)
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
    , ancdNetworkCfg  :: NetworkConfig KademliaParams
    , ancdEkgStore    :: !Metrics.Store
    , ancdTxpMemState :: !(GenericTxpLocalData ext)
    }

allocateNodeContext
    :: forall ext .
      (HasConfiguration, HasNodeConfiguration, HasInfraConfiguration)
    => AllocateNodeContextData ext
    -> TxpGlobalSettings
    -> InitMode NodeContext
allocateNodeContext ancd txpSettings = do
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
    ncSubscriptionKeepAliveTimer <- newTimer $ 30 * 1000000 -- TODO: use slot duration
    let ctx =
            NodeContext
            { ncConnectedPeers = ConnectedPeers peersVar
            , ncLrcContext = LrcContext {..}
            , ncShutdownContext = ShutdownContext ncShutdownFlag
            , ncNodeParams = np
            , ncTxpGlobalSettings = txpSettings
            , ncNetworkConfig = networkConfig
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
