module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    ) where

import           Universum

import           Cardano.Wallet.API.V1.Types (Account, Address, Wallet, WalletId)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))


-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    { pwlGetWalletIds :: m [WalletId]
    , pwlGetWallet    :: WalletId -> m (Maybe Wallet)
    -- * accounts
    , pwlGetAccounts  :: WalletId -> m [Account]
    -- * addresses
    , pwlGetAddresses :: WalletId -> m [Address]
    }

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion    :: WalletDiffusion
    }

