{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the API defined in "Pos.Node.API".
module Cardano.Node.API where

import           Universum

import           Control.Concurrent.STM (orElse, retry)
import           Control.Lens (lens, to)
import           Data.Time.Units (toMicroseconds)
import qualified Paths_cardano_sl_node as Paths
import           Servant
import qualified Data.Text as Text

import Pos.Util.CompileInfo (CompileTimeInfo, ctiGitRevision)
import           Pos.Chain.Block (LastKnownHeader, LastKnownHeaderTag)
import           Pos.Chain.Update (UpdateConfiguration, withUpdateConfiguration)
import           Pos.DB.GState.Lock (Priority (..), StateLock,
                     withStateLockNoMetrics)
import qualified Pos.DB.Rocks.Functions as DB
import qualified Pos.Infra.Slotting.Util as Slotting
import           Pos.Util (HasLens (..), HasLens')
                     -- mainBlockSlot, prevBlockL)
import           Ntp.Client (NtpStatus (..))
import           Ntp.Packet (NtpOffset)
import           Pos.Chain.Update (curSoftwareVersion)
import qualified Pos.Core as Core
import qualified Pos.DB.Block as DB
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.DB.Class as DB
import qualified Pos.DB.Rocks.Types as DB
import           Pos.Infra.Diffusion.Subscription.Status (ssMap)
import           Pos.Infra.Diffusion.Types
import           Pos.Node.API as Node
import           Pos.Util.Servant

handlers
    :: Diffusion IO
    -> TVar NtpStatus
    -> StateLock
    -> DB.NodeDBs
    -> LastKnownHeader
    -> Core.Timestamp
    -> Core.SlottingVar
    -> UpdateConfiguration
    -> CompileTimeInfo
    -> ServerT Node.API Handler
handlers d t s n l ts sv uc ci =
    getNodeSettings ci uc ts sv
    :<|> getNodeInfo d t s n l
    :<|> applyUpdate
    :<|> postponeUpdate

getNodeSettings
    :: CompileTimeInfo
    -> UpdateConfiguration
    -> Core.Timestamp
    -> Core.SlottingVar
    -> Handler (WalletResponse NodeSettings)
getNodeSettings compileInfo updateConfiguration timestamp slottingVar = do
    let ctx = SettingsCtx timestamp slottingVar
    slotDuration <-
        mkSlotDuration . fromIntegral <$>
            runReaderT Slotting.getNextEpochSlotDuration ctx

    pure $ single NodeSettings
        { setSlotDuration =
            slotDuration
        , setSoftwareInfo =
            V1 (withUpdateConfiguration updateConfiguration curSoftwareVersion)
        , setProjectVersion =
            V1 Paths.version
        , setGitRevision =
            Text.replace "\n" mempty (ctiGitRevision compileInfo)
        }


data SettingsCtx = SettingsCtx
    { settingsCtxTimestamp   :: Core.Timestamp
    , settingsCtxSlottingVar :: Core.SlottingVar
    }

instance Core.HasSlottingVar SettingsCtx where
    slottingTimestamp = lens settingsCtxTimestamp (\s t -> s { settingsCtxTimestamp = t })
    slottingVar = lens settingsCtxSlottingVar (\s t -> s { settingsCtxSlottingVar = t })

{-
-- from Cardano.WalletLayer.Kernel.Settings:
getNodeSettings :: MonadIO m => Kernel.PassiveWallet -> m V1.NodeSettings
getNodeSettings w = liftIO $
    V1.NodeSettings
        <$> (mkSlotDuration <$> Node.getNextEpochSlotDuration node)
        <*> (V1 <$> Node.curSoftwareVersion node)
        <*> pure (V1 version)
        <*> (mkGitRevision <$> Node.compileInfo node)
  where
    mkSlotDuration :: Millisecond -> V1.SlotDuration
    mkSlotDuration = V1.mkSlotDuration . fromIntegral

    mkGitRevision :: CompileTimeInfo -> Text
    mkGitRevision = T.replace "\n" mempty . ctiGitRevision

    node :: NodeStateAdaptor IO
    node = w ^. Kernel.walletNode

-- from Cardano.Wallet.Kernel.NodeSTateAdapter
defaultGetNextEpochSlotDuration :: MonadIO m => WithNodeState m Millisecond
defaultGetNextEpochSlotDuration = Slotting.getNextEpochSlotDuration

-- from Pos.Infra.Slotting.Util:
-- | Get last known slot duration.
getNextEpochSlotDuration
    :: (MonadSlotsData ctx m)
    => m Millisecond
getNextEpochSlotDuration =
    esdSlotDuration . snd <$> getCurrentNextEpochSlottingDataM

-- from Pos.Core.SLotting.Class
type MonadSlotsData ctx m =
    ( MonadReader ctx m
    , HasSlottingVar ctx
    , MonadIO m
    )

-- | System start and slotting data
class HasSlottingVar ctx where
    slottingTimestamp :: Lens' ctx Timestamp
    slottingVar       :: Lens' ctx SlottingVar

type SlottingVar = TVar SlottingData

-}

applyUpdate :: Handler NoContent
applyUpdate = undefined

postponeUpdate :: Handler NoContent
postponeUpdate = undefined

getNodeInfo
    :: Diffusion IO
    -> TVar NtpStatus
    -> StateLock
    -> DB.NodeDBs
    -> LastKnownHeader
    -- endpoint parameters
    -> ForceNtpCheck
    -> Handler (WalletResponse NodeInfo)
getNodeInfo diffusion ntpTvar stateLock nodeDBs lastknownHeader forceNtp = liftIO $ do
    single <$> do
        let r = InfoCtx
                { infoCtxStateLock = stateLock
                , infoCtxNodeDbs = nodeDBs
                , infoCtxLastKnownHeader = lastknownHeader
                }
        (mbNodeHeight, localHeight) <-
            runReaderT getNodeSyncProgress r
            -- view defaultSyncProgress impl

        localTimeInformation <-
            liftIO $ defaultGetNtpDrift ntpTvar forceNtp
            -- Node.getNtpDrift node forceNtp

        subscriptionStatus <-
            readTVarIO $ ssMap (subscriptionStates diffusion)

        pure NodeInfo
            { nfoSyncProgress =
                v1SyncPercentage mbNodeHeight localHeight
            , nfoBlockchainHeight =
                mkBlockchainHeight <$> mbNodeHeight
            , nfoLocalBlockchainHeight =
                mkBlockchainHeight localHeight
            , nfoLocalTimeInformation =
                localTimeInformation
            , nfoSubscriptionStatus =
                subscriptionStatus
            }

data InfoCtx = InfoCtx
    { infoCtxStateLock       :: StateLock
    , infoCtxNodeDbs         :: DB.NodeDBs
    , infoCtxLastKnownHeader :: LastKnownHeader
    }

instance HasLens DB.NodeDBs InfoCtx DB.NodeDBs where
    lensOf = lens infoCtxNodeDbs (\i s -> i { infoCtxNodeDbs = s })

instance HasLens StateLock InfoCtx StateLock where
    lensOf = lens infoCtxStateLock (\i s -> i { infoCtxStateLock = s })

instance HasLens LastKnownHeaderTag InfoCtx LastKnownHeader where
    lensOf = lens infoCtxLastKnownHeader (\i s -> i { infoCtxLastKnownHeader = s })

instance
    (HasLens' r DB.NodeDBs, MonadCatch m, MonadIO m)
    => DB.MonadDBRead (ReaderT r m)
  where
    dbGet = DB.dbGetDefault
    dbIterSource = DB.dbIterSourceDefault
    dbGetSerBlock = DB.dbGetSerBlockRealDefault
    dbGetSerUndo = DB.dbGetSerUndoRealDefault
    dbGetSerBlund = DB.dbGetSerBlundRealDefault

getNodeSyncProgress
    ::
    ( MonadIO m
    , MonadMask m
    , DB.MonadDBRead m
    , DB.MonadRealDB r m
    , HasLens' r StateLock
    , HasLens LastKnownHeaderTag r LastKnownHeader
    )
    => m (Maybe Core.BlockCount, Core.BlockCount)
getNodeSyncProgress = do
    (globalHeight, localHeight) <- withStateLockNoMetrics LowPriority $ \_ -> do
        -- We need to grab the localTip again as '_localTip' has type
        -- 'HeaderHash' but we cannot grab the difficulty out of it.
        headerRef <- view (lensOf @LastKnownHeaderTag)
        localTip  <- DB.getTipHeader
        mbHeader <- atomically $ readTVar headerRef `orElse` pure Nothing
        pure (view (Core.difficultyL . to Core.getChainDifficulty) <$> mbHeader
             ,view (Core.difficultyL . to Core.getChainDifficulty) localTip
             )
    return (max localHeight <$> globalHeight, localHeight)

-- | Computes the V1 'SyncPercentage' out of the global & local blockchain heights.
v1SyncPercentage :: Maybe Core.BlockCount -> Core.BlockCount -> SyncPercentage
v1SyncPercentage nodeHeight walletHeight =
    mkSyncPercentage $ case nodeHeight of
        Nothing ->
            0
        Just nd | walletHeight >= nd ->
            100
        Just nd ->
            floor @Double $
                ( fromIntegral walletHeight
                / max 1.0 (fromIntegral nd)
                ) * 100.0

-- | Get the difference between NTP time and local system time, nothing if the
-- NTP server couldn't be reached in the last 30min.
--
-- Note that one can force a new query to the NTP server in which case, it may
-- take up to 30s to resolve.
--
-- Copied from Cardano.Wallet.Kernel.NodeStateAdapter
defaultGetNtpDrift
    :: TVar NtpStatus
    -> ForceNtpCheck
    -> IO TimeInfo
defaultGetNtpDrift tvar ntpCheck = mkTimeInfo <$> case ntpCheck of
    ForceNtpCheck -> do
        forceNtpCheck
        getNtpOffset blockingLookupNtpOffset
    NoNtpCheck ->
        getNtpOffset nonBlockingLookupNtpOffset
  where
    forceNtpCheck =
        atomically $ writeTVar tvar NtpSyncPending

    getNtpOffset lookupNtpOffset =
        atomically (readTVar tvar >>= lookupNtpOffset)

    mkTimeInfo :: Maybe NtpOffset -> TimeInfo
    mkTimeInfo =
        TimeInfo . fmap (mkLocalTimeDifference . toMicroseconds)

    blockingLookupNtpOffset
        :: NtpStatus
        -> STM (Maybe NtpOffset)
    blockingLookupNtpOffset = \case
        NtpSyncPending     -> retry
        NtpDrift offset    -> pure (Just offset)
        NtpSyncUnavailable -> pure Nothing

    nonBlockingLookupNtpOffset
        :: NtpStatus
        -> STM (Maybe NtpOffset)
    nonBlockingLookupNtpOffset = \case
        NtpSyncPending     -> pure Nothing
        NtpDrift offset    -> pure (Just offset)
        NtpSyncUnavailable -> pure Nothing
