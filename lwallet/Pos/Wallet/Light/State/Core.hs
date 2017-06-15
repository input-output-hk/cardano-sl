{-# LANGUAGE TypeFamilies #-}

-- | Keeps instance of `MonadGState`.

module Pos.Wallet.Light.State.Core
       ( GStateCoreWalletRedirect
       , runGStateCoreWalletRedirect
       , gsAdoptedBVDataWallet
       ) where

import           Universum

import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import qualified Ether

import qualified Pos.Constants                as Const
import           Pos.Core                     (BlockVersionData)
import           Pos.DB.Class                 (MonadGState (..))

data GStateCoreWalletRedirectTag

type GStateCoreWalletRedirect =
    Ether.TaggedTrans GStateCoreWalletRedirectTag IdentityT

runGStateCoreWalletRedirect :: GStateCoreWalletRedirect m a -> m a
runGStateCoreWalletRedirect = coerce

-- Currently 'WalletState' doesn't maintain block version data.
gsAdoptedBVDataWallet :: Monad m => m BlockVersionData
gsAdoptedBVDataWallet = pure Const.genesisBlockVersionData

instance (Monad m, t ~ IdentityT) =>
         MonadGState (Ether.TaggedTrans GStateCoreWalletRedirectTag t m) where
    gsAdoptedBVData = gsAdoptedBVDataWallet
