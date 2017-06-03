{-# LANGUAGE TypeFamilies #-}

-- | Keeps instance of `MonadGState`.

module Pos.Wallet.State.Core
       ( GStateCoreWalletRedirect
       , runGStateCoreWalletRedirect
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import qualified Pos.Constants                as Const
import           Pos.DB.Class                 (MonadGState (..))
import           Pos.Wallet.State.Acidic      (WalletState)

data GStateCoreWalletRedirectTag

type GStateCoreWalletRedirect =
    Ether.TaggedTrans GStateCoreWalletRedirectTag IdentityT

runGStateCoreWalletRedirect :: GStateCoreWalletRedirect m a -> m a
runGStateCoreWalletRedirect = coerce

-- Currently 'WalletState' doesn't maintain block version data.
instance (Monad m, t ~ IdentityT, Ether.MonadReader' WalletState m) =>
         MonadGState (Ether.TaggedTrans GStateCoreWalletRedirectTag t m) where
    gsAdoptedBVData = pure Const.genesisBlockVersionData
