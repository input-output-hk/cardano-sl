{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Wallet.WalletLayer.Kernel
    ( bracketPassiveWallet
    , bracketActiveWallet
    ) where

import           Universum

import           System.Wlog (Severity)

import           Cardano.Wallet.WalletLayer.Types (ActiveWalletLayer (..), PassiveWalletLayer (..))

import qualified Cardano.Wallet.Kernel as Kernel
import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))

-- | Initialize the passive wallet.
-- The passive wallet cannot send new transactions.
bracketPassiveWallet
    :: forall m n a. (MonadMask m, Monad n)
    => (Severity -> Text -> IO ())
    -> (PassiveWalletLayer n -> m a) -> m a
bracketPassiveWallet logFunction =
    bracket
        (Kernel.bracketPassiveWallet logFunction passiveWalletLayer)
        (\_ -> return ())
  where
    -- | TODO(ks): Currently not implemented!
    passiveWalletLayer _wallet =
        pure $ PassiveWalletLayer
            { pwlGetWalletIds  = error "Not implemented!"
            , pwlGetWallet     = error "Not implemented!"
            , pwlGetAccounts   = error "Not implemented!"
            , pwlGetAddresses  = error "Not implemented!"
            }

-- | Initialize the active wallet.
-- The active wallet is allowed all.
bracketActiveWallet
    :: forall m n a. (MonadMask m, Monad n)
    => PassiveWalletLayer n
    -> WalletDiffusion
    -> (ActiveWalletLayer n -> m a) -> m a
bracketActiveWallet walletPassiveLayer walletDiffusion =
    bracket
      (return ActiveWalletLayer{..})
      (\_ -> return ())

