{-# LANGUAGE LambdaCase #-}

-- | This module implements the API defined in "Pos.Node.API".
module Cardano.Node.API where

import Universum

import Servant
import Control.Concurrent.STM (retry, orElse)
import Data.Time.Units (toMicroseconds)
import Control.Lens (to)

import qualified Pos.DB.Rocks.Functions as DB
import           Pos.Util (HasLens', HasLens (..), lensOf')
import           Pos.DB.GState.Lock (Priority(..), StateLock, withStateLockNoMetrics)
import           Pos.Chain.Block (LastKnownHeader,
                     LastKnownHeaderTag )--, MainBlock, blockHeader, headerHash,
                     -- mainBlockSlot, prevBlockL)
import qualified Pos.DB.BlockIndex as DB
import qualified Pos.DB.Class as DB
import qualified Pos.Core as Core
import qualified Pos.DB.Rocks.Types as DB
import qualified Pos.DB.Block as DB
import Pos.Node.API
import Pos.Util.Servant
import           Ntp.Packet (NtpOffset)
import           Ntp.Client (NtpStatus (..))
import           Pos.Infra.Diffusion.Subscription.Status (SubscriptionStatus,
                     ssMap)
import           Pos.Infra.Diffusion.Types

-- import           Cardano.Wallet.API.Response (WalletResponse, single)
-- import qualified Cardano.Wallet.API.V1.Info as Info
-- import           Cardano.Wallet.API.V1.Types (ForceNtpCheck, NodeInfo)
-- import           Cardano.Wallet.WalletLayer (ActiveWalletLayer)
-- import qualified Cardano.Wallet.WalletLayer as WalletLayer

-- handlers :: _ -> ServerT NodeInfo Handler
handlers = getNodeInfo

getNodeInfo
    :: Diffusion IO
    -> TVar NtpStatus
    -- endpoint parameters
    -> ForceNtpCheck
    -> Handler (WalletResponse NodeInfo)
getNodeInfo diffusion ntpTvar forceNtp = liftIO $ do
    single <$> do
        let r = InfoCtx
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

instance HasLens DB.NodeDBs InfoCtx DB.NodeDBs where
    lensOf = undefined

instance HasLens StateLock InfoCtx StateLock where
    lensOf = undefined

instance HasLens LastKnownHeaderTag InfoCtx LastKnownHeader where
    lensOf = undefined

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

{- from Cardano.Wallet.WalletLayer.Kernel.Info:

getNodeInfo :: MonadIO m => Kernel.ActiveWallet -> V1.ForceNtpCheck -> m V1.NodeInfo
getNodeInfo aw ntpCheckBehavior = liftIO $ do
    (mbNodeHeight, localHeight) <- Node.getNodeSyncProgress node Node.NotYetLocked
    V1.NodeInfo
      <$> (pure $ v1SyncPercentage mbNodeHeight localHeight)
      <*> (pure $ V1.mkBlockchainHeight <$> mbNodeHeight)
      <*> (pure $ V1.mkBlockchainHeight localHeight)
      <*> (Node.getNtpDrift node ntpCheckBehavior)
      <*> (walletGetSubscriptionStatus (Kernel.walletDiffusion aw))
  where
    node :: NodeStateAdaptor IO
    node = pw ^. Kernel.walletNode

    pw :: Kernel.PassiveWallet
    pw = Kernel.walletPassive aw

-- from Cardano.Wallet.Kernel.NodeStateAdapter:
defaultSyncProgress :: (MonadIO m, MonadMask m, NodeConstraints)
                    => LockContext
                    -> Lock (WithNodeState m)
                    -> WithNodeState m (Maybe BlockCount, BlockCount)
defaultSyncProgress lockContext lock = do
    (globalHeight, localHeight) <- lock lockContext $ \_localTipHash -> do
        -- We need to grab the localTip again as '_localTip' has type
        -- 'HeaderHash' but we cannot grab the difficulty out of it.
        headerRef <- view (lensOf @LastKnownHeaderTag)
        localTip  <- getTipHeader
        mbHeader <- atomically $ readTVar headerRef `orElse` pure Nothing
        pure (view (difficultyL . to getChainDifficulty) <$> mbHeader
             ,view (difficultyL . to getChainDifficulty) localTip
             )
    return (max localHeight <$> globalHeight, localHeight)

-- from Cardano.Wallet.Kernel.Diffusion:
-- | Extract necessary functionality from the full diffusion layer
fromDiffusion :: (forall a. m a -> IO a)
              -> Diffusion m
              -> WalletDiffusion
fromDiffusion nat d = WalletDiffusion {
      walletSendTx                = nat . sendTx d
    , walletGetSubscriptionStatus = readTVarIO $ ssMap (subscriptionStates d)
    }
-}

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
