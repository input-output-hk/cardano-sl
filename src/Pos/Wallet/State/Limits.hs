{-# LANGUAGE TypeFamilies #-}

-- | Keeps instance of `MonadDBLimits`.

module Pos.Wallet.State.Limits
    ( DbLimitsWalletRedirect
    , runDbLimitsWalletRedirect
    ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import qualified Pos.Constants                as Const
import           Pos.DB.Limits                (MonadDBLimits (..))
import           Pos.Wallet.State.Acidic      (WalletState)
import qualified Pos.Wallet.State.State       as DB

data DbLimitsWalletRedirectTag

type DbLimitsWalletRedirect =
    Ether.TaggedTrans DbLimitsWalletRedirectTag IdentityT

runDbLimitsWalletRedirect :: DbLimitsWalletRedirect m a -> m a
runDbLimitsWalletRedirect = coerce

instance
    (MonadIO m, t ~ IdentityT, Ether.MonadReader' WalletState m) =>
        MonadDBLimits (Ether.TaggedTrans DbLimitsWalletRedirectTag t m)
  where
    getMaxBlockSize = DB.getMaxBlockSize
    getMaxHeaderSize = pure Const.genesisMaxHeaderSize
    getMaxTxSize = pure Const.genesisMaxTxSize
    getMaxProposalSize = pure Const.genesisMaxUpdateProposalSize
