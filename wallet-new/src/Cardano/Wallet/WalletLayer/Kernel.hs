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
            { _pwlCreateWallet  = error "Not implemented!"
            , _pwlGetWalletIds  = error "Not implemented!"
            , _pwlGetWallet     = error "Not implemented!"
            , _pwlUpdateWallet  = error "Not implemented!"
            , _pwlDeleteWallet  = error "Not implemented!"

            , _pwlCreateAccount = error "Not implemented!"
            , _pwlGetAccounts   = error "Not implemented!"
            , _pwlGetAccount    = error "Not implemented!"
            , _pwlUpdateAccount = error "Not implemented!"
            , _pwlDeleteAccount = error "Not implemented!"

            , _pwlGetAddresses  = error "Not implemented!"
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

