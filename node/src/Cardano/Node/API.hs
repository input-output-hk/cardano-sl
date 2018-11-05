{-# LANGUAGE LambdaCase #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the API defined in "Pos.Node.API".
module Cardano.Node.API where

import           Universum

import           Control.Concurrent.STM (orElse, retry)
import           Control.Lens (lens, to)
import           Data.Time.Units (toMicroseconds)
import           Servant

import           Pos.Chain.Block (LastKnownHeader, LastKnownHeaderTag)
import           Pos.DB.GState.Lock (Priority (..), StateLock,
                     withStateLockNoMetrics)
import qualified Pos.DB.Rocks.Functions as DB
import           Pos.Util (HasLens (..), HasLens')
                     -- mainBlockSlot, prevBlockL)
import           Ntp.Client (NtpStatus (..))
import           Ntp.Packet (NtpOffset)
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
    -> ServerT Node.API Handler
handlers d t s n l =
    getNodeSettings
    :<|> getNodeInfo d t s n l
    :<|> applyUpdate
    :<|> postponeUpdate

getNodeSettings :: Handler (WalletResponse NodeSettings)
getNodeSettings = undefined

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
