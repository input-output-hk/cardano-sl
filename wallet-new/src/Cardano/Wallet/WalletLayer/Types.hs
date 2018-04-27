{-# LANGUAGE StrictData #-}

module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    -- * Getters
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

    , createTx
    , getTxs
    , estimateFees

    , getSettings

    , getInfo

    ) where


import           Universum

import           Control.Lens (makeLenses)

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, AccountUpdate, AddressValidity,
                                              EstimatedFees, NewAccount, NewAddress, NewWallet,
                                              NodeInfo, NodeSettings, Payment, Transaction, V1,
                                              Wallet, WalletAddress, WalletId, WalletUpdate)
import           Pos.Core (Address, TxAux)
import           Ntp.Client (NtpStatus)

import           Cardano.Wallet.Kernel.Diffusion (WalletDiffusion (..))
import           Cardano.Wallet.WalletLayer.Error

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet   :: NewWallet -> m (Either CreateWalletException Wallet)
    , _pwlGetWalletIds   :: m (Either GetWalletIdsException [WalletId])
    , _pwlGetWallet      :: WalletId -> m (Either GetWalletException Wallet)
    , _pwlUpdateWallet   :: WalletId -> WalletUpdate -> m (Either UpdateWalletException Wallet)
    , _pwlDeleteWallet   :: WalletId -> m (Either DeleteWalletException ())
    -- * accounts
    , _pwlCreateAccount  :: WalletId -> NewAccount -> m (Either CreateAccountException Account)
    , _pwlGetAccounts    :: WalletId -> m (Either GetAccountsException [Account])
    , _pwlGetAccount     :: WalletId -> AccountIndex -> m (Either GetAccountException Account)
    , _pwlUpdateAccount  :: WalletId -> AccountIndex -> AccountUpdate -> m (Either UpdateAccountException Account)
    , _pwlDeleteAccount  :: WalletId -> AccountIndex -> m (Either DeleteAccountException ())
    -- * addresses
    , _pwlCreateAddress  :: NewAddress -> m (Either CreateAddressException WalletAddress)
    , _pwlGetAddresses   :: WalletId -> Maybe AccountIndex -> m (Either GetAddressesException [WalletAddress])
    , _pwlIsAddressValid :: WalletAddress -> m (Either IsValidAddressException AddressValidity)
    -- * transactions
    , _pwlCreateTx       :: (TxAux -> m Bool) -> Payment -> m (Either CreateTxException Transaction)
    , _pwlGetTxs         :: Maybe WalletId -> Maybe AccountIndex -> Maybe (V1 Address) -> m (Either GetTxException [Transaction])
    , _pwlEstimateFees   :: Payment -> m (Either FeeEstimateException EstimatedFees)
    -- * settings
    , _pwlGetSettings    :: m (Either GetSettingsException NodeSettings)
    -- * info
    , _pwlGetInfo        :: TVar NtpStatus -> m (Either GetInfoException NodeInfo)
    }

makeLenses ''PassiveWalletLayer

------------------------------------------------------------
-- Passive wallet layer getters
------------------------------------------------------------

createWallet :: forall m. PassiveWalletLayer m -> NewWallet -> m (Either CreateWalletException Wallet)
createWallet pwl = pwl ^. pwlCreateWallet

getWalletIds :: forall m. PassiveWalletLayer m -> m (Either GetWalletIdsException [WalletId])
getWalletIds pwl = pwl ^. pwlGetWalletIds

getWallet :: forall m. PassiveWalletLayer m -> WalletId -> m (Either GetWalletException Wallet)
getWallet pwl = pwl ^. pwlGetWallet

updateWallet :: forall m. PassiveWalletLayer m -> WalletId -> WalletUpdate -> m (Either UpdateWalletException Wallet)
updateWallet pwl = pwl ^. pwlUpdateWallet

deleteWallet :: forall m. PassiveWalletLayer m -> WalletId -> m (Either DeleteWalletException ())
deleteWallet pwl = pwl ^. pwlDeleteWallet


createAccount :: forall m. PassiveWalletLayer m -> WalletId -> NewAccount -> m (Either CreateAccountException Account)
createAccount pwl = pwl ^. pwlCreateAccount

getAccounts :: forall m. PassiveWalletLayer m -> WalletId -> m (Either GetAccountsException [Account])
getAccounts pwl = pwl ^. pwlGetAccounts

getAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> m (Either GetAccountException Account)
getAccount pwl = pwl ^. pwlGetAccount

updateAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> AccountUpdate -> m (Either UpdateAccountException Account)
updateAccount pwl = pwl ^. pwlUpdateAccount

deleteAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> m (Either DeleteAccountException ())
deleteAccount pwl = pwl ^. pwlDeleteAccount


createAddress :: forall m. PassiveWalletLayer m -> NewAddress -> m (Either CreateAddressException WalletAddress)
createAddress pwl = pwl ^. pwlCreateAddress

getAddresses :: forall m. PassiveWalletLayer m -> WalletId -> Maybe AccountIndex -> m (Either GetAddressesException [WalletAddress])
getAddresses pwl = pwl ^. pwlGetAddresses

isAddressValid :: forall m. PassiveWalletLayer m -> WalletAddress -> m (Either IsValidAddressException AddressValidity)
isAddressValid pwl = pwl ^. pwlIsAddressValid


createTx :: forall m. PassiveWalletLayer m -> (TxAux -> m Bool) -> Payment -> m (Either CreateTxException Transaction)
createTx pwl = pwl ^. pwlCreateTx

getTxs :: forall m. PassiveWalletLayer m -> Maybe WalletId -> Maybe AccountIndex -> Maybe (V1 Address) -> m (Either GetTxException [Transaction])
getTxs pwl = pwl ^. pwlGetTxs

estimateFees :: forall m. PassiveWalletLayer m -> Payment -> m (Either FeeEstimateException EstimatedFees)
estimateFees pwl = pwl ^. pwlEstimateFees


getSettings :: forall m. PassiveWalletLayer m -> m (Either GetSettingsException NodeSettings)
getSettings pwl = pwl ^. pwlGetSettings


getInfo :: forall m. PassiveWalletLayer m -> TVar NtpStatus -> m (Either GetInfoException NodeInfo)
getInfo pwl = pwl ^. pwlGetInfo

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

