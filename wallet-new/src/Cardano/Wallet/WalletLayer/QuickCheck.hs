module Cardano.Wallet.WalletLayer.QuickCheck where

import           Universum

import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))
import           Cardano.Wallet.Orphans.Arbitrary () -- Arbitrary instances

import           Test.QuickCheck (Arbitrary, arbitrary, generate)

-- | Allocation of the wallet resources for the arbitrary wallet.
kernelWalletLayer :: forall m. (MonadIO m) => PassiveWalletLayer m
kernelWalletLayer = PassiveWalletLayer
    { pwlGetWalletAddresses  = liftedGen
    , pwlGetWalletMeta       = \_ -> liftedGen
    }

-- | A utility function.
liftedGen :: forall m a. (MonadIO m, Arbitrary a) => m a
liftedGen = liftIO . generate $ arbitrary


