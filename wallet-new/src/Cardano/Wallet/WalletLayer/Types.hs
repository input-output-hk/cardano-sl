module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Lenses
    , pwlCreateWallet
    , pwlGetWalletIds
    , pwlGetWallet
    , pwlUpdateWallet
    , pwlDeleteWallet
    , pwlGetAccounts
    , pwlGetAccount
    , pwlGetAddresses
    ) where

import           Universum
import           Control.Lens (makeLenses)

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, Address, NewWallet, Wallet,
                                              WalletId, WalletUpdate)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))


-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet :: NewWallet -> m (Maybe Wallet)
    , _pwlGetWalletIds :: m [WalletId]
    , _pwlGetWallet    :: WalletId -> m (Maybe Wallet)
    , _pwlUpdateWallet :: WalletId -> WalletUpdate -> m (Maybe Wallet)
    , _pwlDeleteWallet :: WalletId -> m Bool
    -- * accounts
    , _pwlGetAccounts  :: WalletId -> m [Account]
    , _pwlGetAccount   :: WalletId -> AccountIndex -> m (Maybe Account)
    -- * addresses
    , _pwlGetAddresses :: WalletId -> m [Address]
    }

makeLenses ''PassiveWalletLayer

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion    :: WalletDiffusion
    }

