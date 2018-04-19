{-# LANGUAGE StrictData #-}

module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    , WalletLayerResponse
    , monadThrowToEither
    -- * Getters
    -- TODO(ks): Should we expose getters only and
    -- use lenses for the modification or is that confusing?
    , createWallet
    , getWalletIds
    , getWallet
    , updateWallet
    , deleteWallet

    , createAccount
    , getAccounts
    , getAccount
    , updateAccount
    , deleteAccount

    , createAddress
    , getAddresses
    , isAddressValid

    , addTx
    , getTxs

    , setWalletRestoring
    , unsetWalletRestoring
    , isWalletRestoring

    , getSyncState
    ) where


import           Universum

import           Control.Exception.Safe (try)
import           Control.Lens (makeLenses)

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, AccountUpdate, AddressValidity,
                                              NewAccount, NewAddress, NewWallet, SyncState,
                                              Transaction, Wallet, WalletAddress, WalletId,
                                              WalletRestorationStatus, WalletUpdate)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.WalletLayer.Error (WalletLayerError (..))

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- The response for the @WalletPassiveLayer@.
type WalletLayerResponse m a = m (Either WalletLayerError a)

-- A utility function for transforming between representations.
monadThrowToEither :: forall m a. (MonadCatch m) => m a -> WalletLayerResponse m a
monadThrowToEither = try

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet          :: NewWallet -> WalletLayerResponse m Wallet
    , _pwlGetWalletIds          :: WalletLayerResponse m [WalletId]
    , _pwlGetWallet             :: WalletId -> WalletLayerResponse m Wallet
    , _pwlUpdateWallet          :: WalletId -> WalletUpdate -> WalletLayerResponse m Wallet
    , _pwlDeleteWallet          :: WalletId -> WalletLayerResponse m ()
    -- * accounts
    , _pwlCreateAccount         :: WalletId -> NewAccount -> WalletLayerResponse m Account
    , _pwlGetAccounts           :: WalletId -> WalletLayerResponse m [Account]
    , _pwlGetAccount            :: WalletId -> AccountIndex -> WalletLayerResponse m Account
    , _pwlUpdateAccount         :: WalletId -> AccountIndex -> AccountUpdate -> WalletLayerResponse m Account
    , _pwlDeleteAccount         :: WalletId -> AccountIndex -> WalletLayerResponse m ()
    -- * addresses
    , _pwlCreateAddress         :: NewAddress -> WalletLayerResponse m WalletAddress
    , _pwlGetAddresses          :: WalletId -> Maybe AccountIndex -> WalletLayerResponse m [WalletAddress]
    , _pwlIsAddressValid        :: WalletAddress -> WalletLayerResponse m AddressValidity
    -- * transactions
    -- We can just add them and fetch them.
    , _pwlAddTx                 :: WalletId -> AccountIndex -> Transaction -> WalletLayerResponse m ()
    , _pwlGetTxs                :: WalletId -> AccountIndex -> WalletLayerResponse m [Transaction]
    -- * required for txp
    , _pwlSetWalletRestoring    :: WalletId -> WalletLayerResponse m ()
    , _pwlUnsetWalletRestoring  :: WalletId -> WalletLayerResponse m ()
    , _pwlIsWalletRestoring     :: WalletId -> WalletLayerResponse m WalletRestorationStatus
    -- * utxo
    -- TODO(ks): Can we hide the rest of internals?
    -- , _getWalletUtxo            :: WalletId -> WalletLayerResponse m Utxo
    -- , _getAccountUtxo           :: AccountIndex -> WalletLayerResponse m Utxo
    -- , _getAddressUtxo           :: WalletAddress -> WalletLayerResponse m Utxo

    , _pwlGetSyncState          :: WalletId -> WalletLayerResponse m SyncState
    }

makeLenses ''PassiveWalletLayer

------------------------------------------------------------
-- Passive wallet layer getters
------------------------------------------------------------

createWallet :: forall m. PassiveWalletLayer m -> NewWallet -> WalletLayerResponse m Wallet
createWallet pwl = pwl ^. pwlCreateWallet

getWalletIds :: forall m. PassiveWalletLayer m -> WalletLayerResponse m [WalletId]
getWalletIds pwl = pwl ^. pwlGetWalletIds

getWallet :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m Wallet
getWallet pwl = pwl ^. pwlGetWallet

updateWallet :: forall m. PassiveWalletLayer m -> WalletId -> WalletUpdate -> WalletLayerResponse m Wallet
updateWallet pwl = pwl ^. pwlUpdateWallet

deleteWallet :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m ()
deleteWallet pwl = pwl ^. pwlDeleteWallet


createAccount :: forall m. PassiveWalletLayer m -> WalletId -> NewAccount -> WalletLayerResponse m Account
createAccount pwl = pwl ^. pwlCreateAccount

getAccounts :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m [Account]
getAccounts pwl = pwl ^. pwlGetAccounts

getAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> WalletLayerResponse m Account
getAccount pwl = pwl ^. pwlGetAccount

updateAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> AccountUpdate -> WalletLayerResponse m Account
updateAccount pwl = pwl ^. pwlUpdateAccount

deleteAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> WalletLayerResponse m ()
deleteAccount pwl = pwl ^. pwlDeleteAccount


createAddress :: forall m. PassiveWalletLayer m -> NewAddress -> WalletLayerResponse m WalletAddress
createAddress pwl = pwl ^. pwlCreateAddress

getAddresses :: forall m. PassiveWalletLayer m -> WalletId -> Maybe AccountIndex -> WalletLayerResponse m [WalletAddress]
getAddresses pwl = pwl ^. pwlGetAddresses

isAddressValid :: forall m. PassiveWalletLayer m -> WalletAddress -> WalletLayerResponse m AddressValidity
isAddressValid pwl = pwl ^. pwlIsAddressValid


addTx :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> Transaction -> WalletLayerResponse m ()
addTx pwl = pwl ^. pwlAddTx

getTxs :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> WalletLayerResponse m [Transaction]
getTxs pwl = pwl ^. pwlGetTxs


setWalletRestoring :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m ()
setWalletRestoring pwl = pwl ^. pwlSetWalletRestoring

unsetWalletRestoring :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m ()
unsetWalletRestoring pwl = pwl ^. pwlUnsetWalletRestoring

isWalletRestoring :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m WalletRestorationStatus
isWalletRestoring pwl = pwl ^. pwlIsWalletRestoring


getSyncState :: forall m. PassiveWalletLayer m -> WalletId -> WalletLayerResponse m SyncState
getSyncState pwl = pwl ^. pwlGetSyncState

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m

      -- | The wallet diffusion layer
    , walletDiffusion    :: WalletDiffusion
    }

