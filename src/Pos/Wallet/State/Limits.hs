{-# LANGUAGE UndecidableInstances #-}

-- | Keeps instance of `MonadDBLimits`.

module Pos.Wallet.State.Limits () where

import           Universum

import qualified Pos.Constants           as Const
import           Pos.DB.Limits           (MonadDBLimits (..))
import           Pos.Wallet.State.Holder (WalletDB)
import qualified Pos.Wallet.State.State  as DB

instance MonadIO m => MonadDBLimits (WalletDB m) where
    getMaxBlockSize = DB.getMaxBlockSize
    getMaxHeaderSize = pure Const.genesisMaxHeaderSize
    getMaxTxSize = pure Const.genesisMaxTxSize
    getMaxProposalSize = pure Const.genesisMaxUpdateProposalSize
