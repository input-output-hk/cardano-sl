module Cardano.Wallet.WalletLayer.QuickCheck where

import           Universum

import           Cardano.Wallet.WalletLayer (PassiveWalletLayer (..))
import           Cardano.Wallet.Orphans.Arbitrary () -- Arbitrary instances

import           Test.QuickCheck (Arbitrary, arbitrary, generate)

-- | Allocation of the wallet resources for the arbitrary wallet.
quickCheckWalletLayer :: forall m. (MonadIO m) => PassiveWalletLayer m
quickCheckWalletLayer = PassiveWalletLayer
    { pwlGetWalletAddresses  = liftedGen
    , pwlGetWalletMeta       = \_ -> liftedGen
    -- | We don't require messages at this point.
    , pwlWalletLogMessage    = \_ _ -> pure ()
    }

-- | A utility function.
liftedGen :: forall m a. (MonadIO m, Arbitrary a) => m a
liftedGen = liftIO . generate $ arbitrary


