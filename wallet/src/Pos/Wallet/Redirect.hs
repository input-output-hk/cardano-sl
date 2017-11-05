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

import           Control.Lens          (views)
import           Data.Time.Units       (Millisecond)
import           Ether.Internal        (HasLens (..))
import           System.Wlog           (WithLogger)

import qualified Pos.Context           as PC
import           Pos.Core              (ChainDifficulty, HasConfiguration, difficultyL)
import           Pos.Core.Block        (BlockHeader)
import           Pos.DB.Block          (MonadBlockDB)
import           Pos.DB.DB             (getTipHeader)
import qualified Pos.GState            as GS
import           Pos.Shutdown          (HasShutdownContext, triggerShutdown)
import           Pos.Slotting          (MonadSlots (..), getNextEpochSlotDuration)
import           Pos.Update.Context    (UpdateContext (ucDownloadedUpdate))
import           Pos.Update.Poll.Types (ConfirmedProposalState)
import           Pos.Wallet.WalletMode (MonadBlockchainInfo (..), MonadUpdates (..))

----------------------------------------------------------------------------
-- BlockchainInfo
----------------------------------------------------------------------------

getLastKnownHeader
  :: (PC.MonadLastKnownHeader ctx m, MonadIO m)
  => m (Maybe BlockHeader)
getLastKnownHeader =
    atomically . readTVar =<< view (lensOf @PC.LastKnownHeaderTag)

type BlockchainInfoEnv ctx m =
    ( MonadBlockDB m
    , PC.MonadLastKnownHeader ctx m
    , PC.MonadProgressHeader ctx m
    , MonadReader ctx m
    , HasLens PC.ConnectedPeers ctx PC.ConnectedPeers
    , MonadIO m
    , MonadSlots ctx m
    , HasConfiguration
    )

networkChainDifficultyWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m (Maybe ChainDifficulty)
networkChainDifficultyWebWallet = getLastKnownHeader >>= \case
    Just lh -> do
        thDiff <- view difficultyL <$> getTipHeader
        let lhDiff = lh ^. difficultyL
        return . Just $ max thDiff lhDiff
    Nothing -> pure Nothing

localChainDifficultyWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m ChainDifficulty
localChainDifficultyWebWallet = do
    -- Workaround: Make local chain difficulty monotonic
    prevMaxDifficulty <- fromMaybe 0 <$> GS.getMaxSeenDifficultyMaybe
    currDifficulty <- view difficultyL <$> getTipHeader
    return $ max prevMaxDifficulty currDifficulty

connectedPeersWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
    => m Word
connectedPeersWebWallet = fromIntegral . length <$> do
    PC.ConnectedPeers cp <- view (lensOf @PC.ConnectedPeers)
    atomically (readTVar cp)

blockchainSlotDurationWebWallet
    :: forall ctx m. BlockchainInfoEnv ctx m
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
