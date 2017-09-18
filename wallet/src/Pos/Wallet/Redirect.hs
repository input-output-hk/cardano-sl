{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | This module contains various monadic redirects used by wallet.

module Pos.Wallet.Redirect
       ( MonadBlockchainInfo(..)
       , networkChainDifficultyWebWallet
       , localChainDifficultyWebWallet
       , connectedPeersWebWallet
       , blockchainSlotDurationWebWallet
       , MonadUpdates(..)
       , waitForUpdateWebWallet
       , applyLastUpdateWebWallet
       ) where

import           Universum

import           Control.Lens              (views)
import           Control.Monad.Trans.Maybe (MaybeT (..))
import           Data.Time.Units           (Millisecond)
import           Ether.Internal            (HasLens (..))
import           System.Wlog               (WithLogger)

import           Pos.Block.Core            (BlockHeader)
import qualified Pos.Context               as PC
import           Pos.Core                  (ChainDifficulty, HasConfiguration,
                                            difficultyL, flattenEpochOrSlot,
                                            flattenSlotId, slotSecurityParam)
import           Pos.DB                    (MonadRealDB)
import           Pos.DB.Block              (MonadBlockDB)
import           Pos.DB.DB                 (getTipHeader)
import           Pos.Shutdown              (HasShutdownContext, triggerShutdown)
import           Pos.Slotting              (MonadSlots (..), getNextEpochSlotDuration)
import           Pos.Update.Context        (UpdateContext (ucDownloadedUpdate))
import           Pos.Update.Poll.Types     (ConfirmedProposalState)
import           Pos.Wallet.WalletMode     (MonadBlockchainInfo (..), MonadUpdates (..))

----------------------------------------------------------------------------
-- BlockchainInfo
----------------------------------------------------------------------------

getLastKnownHeader
  :: (PC.MonadLastKnownHeader ssc ctx m, MonadIO m)
  => m (Maybe (BlockHeader ssc))
getLastKnownHeader =
    atomically . readTVar =<< view (lensOf @PC.LastKnownHeaderTag)

type BlockchainInfoEnv ssc ctx m =
    ( MonadBlockDB ssc m
    , PC.MonadLastKnownHeader ssc ctx m
    , PC.MonadProgressHeader ssc ctx m
    , MonadReader ctx m
    , HasLens PC.ConnectedPeers ctx PC.ConnectedPeers
    , MonadIO m
    , MonadRealDB ctx m
    , MonadSlots ctx m
    , HasConfiguration
    )

networkChainDifficultyWebWallet
    :: forall ssc ctx m. BlockchainInfoEnv ssc ctx m
    => m (Maybe ChainDifficulty)
networkChainDifficultyWebWallet = getLastKnownHeader >>= \case
    Just lh -> do
        thDiff <- view difficultyL <$> getTipHeader @ssc
        let lhDiff = lh ^. difficultyL
        return . Just $ max thDiff lhDiff
    Nothing -> runMaybeT $ do
        cSlot <- flattenSlotId <$> MaybeT getCurrentSlot
        th <- lift (getTipHeader @ssc)
        let hSlot = flattenEpochOrSlot th
        when (hSlot + fromIntegral slotSecurityParam <= cSlot) $
            fail "Local tip is outdated"
        return $ th ^. difficultyL

localChainDifficultyWebWallet
    :: forall ssc ctx m. BlockchainInfoEnv ssc ctx m
    => m ChainDifficulty
localChainDifficultyWebWallet = view difficultyL <$> getTipHeader @ssc

connectedPeersWebWallet
    :: forall ssc ctx m. BlockchainInfoEnv ssc ctx m
    => m Word
connectedPeersWebWallet = fromIntegral . length <$> do
    PC.ConnectedPeers cp <- view (lensOf @PC.ConnectedPeers)
    atomically (readTVar cp)

blockchainSlotDurationWebWallet
    :: forall ssc ctx m. BlockchainInfoEnv ssc ctx m
    => m Millisecond
blockchainSlotDurationWebWallet = getNextEpochSlotDuration

----------------------------------------------------------------------------
-- Updates
----------------------------------------------------------------------------

type UpdatesEnv ctx m =
    ( MonadIO m
    , WithLogger m
    , HasShutdownContext ctx
    , MonadReader ctx m
    , HasLens UpdateContext ctx UpdateContext
    )

waitForUpdateWebWallet :: UpdatesEnv ctx m => m ConfirmedProposalState
waitForUpdateWebWallet =
    takeMVar =<< views (lensOf @UpdateContext) ucDownloadedUpdate

applyLastUpdateWebWallet :: UpdatesEnv ctx m => m ()
applyLastUpdateWebWallet = triggerShutdown
