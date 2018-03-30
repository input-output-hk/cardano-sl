{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           Cardano.Wallet.WalletLayer (ActiveWalletLayer (..), PassiveWalletLayer (..))
import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadMask m, Monad n)
    => (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet =
    bracket
        (Kernel.bracketPassiveWallet logFunction passiveWalletLayer)
        (\_ -> return ())
  where
    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer _wallet =
        pure $ PassiveWalletLayer
            { pwlGetWalletAddresses  = error "Not implemented!"
            , pwlGetWalletMeta       = error "Not implemented!"
            }

    -- | Empty log function.
    logFunction _ = pure $ pure ()

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadMask m, Monad n)
    => WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletDiffusion =
    bracket
        (bracketPassiveWallet $ \walletPassiveLayer -> return ActiveWalletLayer {..})
        (\_ -> return ())
