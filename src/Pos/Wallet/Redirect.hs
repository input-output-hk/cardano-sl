{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module contains various monadic redirects used by wallet.

module Pos.Wallet.Redirect
       ( MonadBlockchainInfo(..)
       , BlockchainInfoRedirect
       , runBlockchainInfoRedirect
       , networkChainDifficultyWebWallet
       , localChainDifficultyWebWallet
       , connectedPeersWebWallet
       , blockchainSlotDurationWebWallet
       , MonadUpdates(..)
       , UpdatesRedirect
       , runUpdatesRedirect
       , waitForUpdateWebWallet
       , applyLastUpdateWebWallet
       ) where

import           Universum

import           Control.Concurrent.STM       (tryReadTMVar)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Maybe    (MaybeT (..))
import           Data.Coerce                  (coerce)
import           Data.Time.Units              (Millisecond)
import qualified Ether
import           System.Wlog                  (WithLogger)

import           Pos.Block.Core               (Block, BlockHeader)
import           Pos.Constants                (blkSecurityParam)
import qualified Pos.Context                  as PC
import           Pos.Core                     (ChainDifficulty, difficultyL,
                                               flattenEpochOrSlot, flattenSlotId)
import           Pos.DB                       (MonadRealDB)
import           Pos.DB.Block                 (MonadBlockDB)
import           Pos.DB.DB                    (getTipHeader)
import           Pos.Shutdown                 (MonadShutdownMem, triggerShutdown)
import           Pos.Slotting                 (MonadSlots (..), getLastKnownSlotDuration)
import           Pos.Ssc.Class                (Ssc)
import           Pos.Update.Context           (UpdateContext (ucUpdateSemaphore))
import           Pos.Update.Poll.Types        (ConfirmedProposalState)
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

type BlochainInfoMonad ssc m =
    ( MonadBlockDB ssc m
    , PC.MonadLastKnownHeader ssc m
    , PC.MonadProgressHeader ssc m
    , Ether.MonadReader' PC.ConnectedPeers m
    , MonadIO m
    , MonadRealDB m
    , MonadSlots m
    )

networkChainDifficultyWebWallet
    :: forall ssc m. BlochainInfoMonad ssc m
    => m (Maybe ChainDifficulty)
networkChainDifficultyWebWallet = getLastKnownHeader >>= \case
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

localChainDifficultyWebWallet
    :: forall ssc m. BlochainInfoMonad ssc m
    => m ChainDifficulty
localChainDifficultyWebWallet = downloadHeader >>= \case
    Just dh -> return $ dh ^. difficultyL
    Nothing -> view difficultyL <$> getTipHeader @(Block ssc)

connectedPeersWebWallet
    :: forall ssc m. BlochainInfoMonad ssc m
    => m Word
connectedPeersWebWallet = fromIntegral . length <$> do
    PC.ConnectedPeers cp <- Ether.ask'
    atomically (readTVar cp)

blockchainSlotDurationWebWallet
    :: forall ssc m. BlochainInfoMonad ssc m
    => m Millisecond
blockchainSlotDurationWebWallet = getLastKnownSlotDuration

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
    networkChainDifficulty = networkChainDifficultyWebWallet
    localChainDifficulty = localChainDifficultyWebWallet
    connectedPeers = connectedPeersWebWallet
    blockchainSlotDuration = blockchainSlotDurationWebWallet

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

data UpdatesRedirectTag

type UpdatesRedirect = Ether.TaggedTrans UpdatesRedirectTag IdentityT

runUpdatesRedirect :: UpdatesRedirect m a -> m a
runUpdatesRedirect = coerce

type UpdatesMonad m =
    ( MonadIO m
    , WithLogger m
    , MonadShutdownMem m
    , Ether.MonadReader' UpdateContext m )

waitForUpdateWebWallet :: UpdatesMonad m => m ConfirmedProposalState
waitForUpdateWebWallet = takeMVar =<< Ether.asks' ucUpdateSemaphore

applyLastUpdateWebWallet :: UpdatesMonad m => m ()
applyLastUpdateWebWallet = triggerShutdown

-- | Instance for full node
instance
    ( MonadIO m
    , WithLogger m
    , t ~ IdentityT
    , MonadShutdownMem m
    , Ether.MonadReader' UpdateContext m
    ) => MonadUpdates (Ether.TaggedTrans UpdatesRedirectTag t m)
  where
    waitForUpdate = waitForUpdateWebWallet
    applyLastUpdate = applyLastUpdateWebWallet
