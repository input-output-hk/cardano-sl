{-# LANGUAGE TypeFamilies #-}

-- | Keeps instance of `MonadGStateCore`.

module Pos.Wallet.State.Core
       ( GStateCoreWalletRedirect
       , runGStateCoreWalletRedirect
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import qualified Pos.Constants                as Const
import           Pos.DB.Class                 (MonadGStateCore (..))
import           Pos.Wallet.State.Acidic      (WalletState)

data GStateCoreWalletRedirectTag

type GStateCoreWalletRedirect =
    Ether.TaggedTrans GStateCoreWalletRedirectTag IdentityT

runGStateCoreWalletRedirect :: GStateCoreWalletRedirect m a -> m a
runGStateCoreWalletRedirect = coerce

-- Currently 'WalletState' doesn't maintain block version data.
instance (Monad m, t ~ IdentityT, Ether.MonadReader' WalletState m) =>
         MonadGStateCore (Ether.TaggedTrans GStateCoreWalletRedirectTag t m) where
    gsAdoptedBVData = pure Const.genesisBlockVersionData
