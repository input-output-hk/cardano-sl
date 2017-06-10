{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module contains various monadic redirects used by wallet.

module Pos.Wallet.Redirect
       ( BlockchainInfoRedirect
       , runBlockchainInfoRedirect
       , UpdatesRedirect
       , runUpdatesRedirect
       , WalletRedirects
       , runWalletRedirects
       , liftWalletRedirects
       ) where

import           Universum

import           Control.Concurrent.STM       (tryReadTMVar)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Coerce                  (coerce)
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, BlockHeader)
import           Pos.Client.Txp.Balances      (BalancesRedirect, runBalancesRedirect)
import           Pos.Client.Txp.History       (TxHistoryRedirect, runTxHistoryRedirect)
import           Pos.Constants                (blkSecurityParam)
import qualified Pos.Context                  as PC
import           Pos.Core                     (difficultyL, flattenEpochOrSlot,
                                               flattenSlotId)
import           Pos.DB                       (MonadRealDB)
import           Pos.DB.Block                 (MonadBlockDB)
import           Pos.DB.DB                    (getTipHeader)
import           Pos.Shutdown                 (MonadShutdownMem, triggerShutdown)
import           Pos.Slotting                 (MonadSlots (..), getLastKnownSlotDuration)
import           Pos.Ssc.Class                (Ssc)
import           Pos.Update.Context           (UpdateContext (ucUpdateSemaphore))
import           Pos.Wallet.WalletMode        (MonadBlockchainInfo (..),
                                               MonadUpdates (..))

----------------------------------------------------------------------------
-- BlockchainInfo
----------------------------------------------------------------------------

data BlockchainInfoRedirectTag

type BlockchainInfoRedirect =
    Ether.TaggedTrans BlockchainInfoRedirectTag IdentityT

runBlockchainInfoRedirect :: BlockchainInfoRedirect m a -> m a
runBlockchainInfoRedirect = coerce

getLastKnownHeader
  :: (PC.MonadLastKnownHeader ssc m, MonadIO m)
  => m (Maybe (BlockHeader ssc))
getLastKnownHeader =
    atomically . readTVar =<< Ether.ask @PC.LastKnownHeaderTag

downloadHeader
    :: (Ssc ssc, MonadIO m, PC.MonadProgressHeader ssc m)
    => m (Maybe (BlockHeader ssc))
downloadHeader = do
    atomically . tryReadTMVar =<< Ether.ask @PC.ProgressHeaderTag

-- | Instance for full-node's ContextHolder
instance
    ( MonadBlockDB ssc m
    , t ~ IdentityT
    , PC.MonadLastKnownHeader ssc m
    , PC.MonadProgressHeader ssc m
    , Ether.MonadReader' PC.ConnectedPeers m
    , MonadIO m
    , MonadRealDB m
    , MonadSlots m
    ) => MonadBlockchainInfo (Ether.TaggedTrans BlockchainInfoRedirectTag t m)
  where
    networkChainDifficulty = getLastKnownHeader >>= \case
        Just lh -> do
            thDiff <- view difficultyL <$> getTipHeader @(Block ssc)
            let lhDiff = lh ^. difficultyL
            return . Just $ max thDiff lhDiff
        Nothing -> runMaybeT $ do
            cSlot <- flattenSlotId <$> MaybeT getCurrentSlot
            th <- lift (getTipHeader @(Block ssc))
            let hSlot = flattenEpochOrSlot th
            when (hSlot <= cSlot - blkSecurityParam) $
                fail "Local tip is outdated"
            return $ th ^. difficultyL

    localChainDifficulty = downloadHeader >>= \case
        Just dh -> return $ dh ^. difficultyL
        Nothing -> view difficultyL <$> getTipHeader @(Block ssc)

    connectedPeers = fromIntegral . length <$> do
        PC.ConnectedPeers cp <- Ether.ask'
        atomically (readTVar cp)

    blockchainSlotDuration = getLastKnownSlotDuration

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

data UpdatesRedirectTag

type UpdatesRedirect = Ether.TaggedTrans UpdatesRedirectTag IdentityT

runUpdatesRedirect :: UpdatesRedirect m a -> m a
runUpdatesRedirect = coerce

-- | Instance for full node
instance
    ( MonadIO m
    , WithLogger m
    , t ~ IdentityT
    , MonadShutdownMem m
    , Ether.MonadReader' UpdateContext m
    ) => MonadUpdates (Ether.TaggedTrans UpdatesRedirectTag t m)
  where
    waitForUpdate = takeMVar =<< Ether.asks' ucUpdateSemaphore
    applyLastUpdate = triggerShutdown

----------------------------------------------------------------------------
-- Combination of wallet redirects
----------------------------------------------------------------------------

-- | This is type alias for a stack of monad transformers which add
-- instances of type classes used by wallet.
type WalletRedirects m =
    BlockchainInfoRedirect (
    UpdatesRedirect (
    TxHistoryRedirect (
    BalancesRedirect
    m
    )))

-- | «Runner» for 'WalletRedirects'.
runWalletRedirects :: WalletRedirects m a -> m a
runWalletRedirects =
      runBalancesRedirect
    . runTxHistoryRedirect
    . runUpdatesRedirect
    . runBlockchainInfoRedirect

-- | Lift monadic computation to 'WalletRedirects'.
liftWalletRedirects :: m a -> WalletRedirects m a
liftWalletRedirects = coerce
