{-# LANGUAGE StrictData #-}

module Cardano.Wallet.WalletLayer.Types
    ( PassiveWalletLayer (..)
    , ActiveWalletLayer (..)
    , WalletLayerResponse
    , monadThrowToEither
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

    ) where


import           Universum

import           Control.Exception.Safe (try)
import           Control.Lens (makeLenses)

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, AccountUpdate, AddressValidity,
                                              EstimatedFees, NewAccount, NewAddress, NewWallet,
                                              Payment, Transaction, V1, Wallet, WalletAddress,
                                              WalletId, WalletUpdate)
import           Pos.Core (Address, TxAux)

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
      _pwlCreateWallet   :: NewWallet -> WalletLayerResponse m Wallet
    , _pwlGetWalletIds   :: WalletLayerResponse m [WalletId]
    , _pwlGetWallet      :: WalletId -> WalletLayerResponse m Wallet
    , _pwlUpdateWallet   :: WalletId -> WalletUpdate -> WalletLayerResponse m Wallet
    , _pwlDeleteWallet   :: WalletId -> WalletLayerResponse m ()
    -- * accounts
    , _pwlCreateAccount  :: WalletId -> NewAccount -> WalletLayerResponse m Account
    , _pwlGetAccounts    :: WalletId -> WalletLayerResponse m [Account]
    , _pwlGetAccount     :: WalletId -> AccountIndex -> WalletLayerResponse m Account
    , _pwlUpdateAccount  :: WalletId -> AccountIndex -> AccountUpdate -> WalletLayerResponse m Account
    , _pwlDeleteAccount  :: WalletId -> AccountIndex -> WalletLayerResponse m ()
    -- * addresses
    , _pwlCreateAddress  :: NewAddress -> WalletLayerResponse m WalletAddress
    , _pwlGetAddresses   :: WalletId -> Maybe AccountIndex -> WalletLayerResponse m [WalletAddress]
    , _pwlIsAddressValid :: WalletAddress -> WalletLayerResponse m AddressValidity
    -- * transactions
    , _pwlCreateTx       :: (TxAux -> m Bool) -> Payment -> WalletLayerResponse m Transaction
    , _pwlGetTxs         :: Maybe WalletId -> Maybe AccountIndex -> Maybe (V1 Address) -> WalletLayerResponse m [Transaction]
    , _pwlEstimateFees   :: Payment -> WalletLayerResponse m EstimatedFees
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


createTx :: forall m. PassiveWalletLayer m -> (TxAux -> m Bool) -> Payment -> WalletLayerResponse m Transaction
createTx pwl = pwl ^. pwlCreateTx

getTxs :: forall m. PassiveWalletLayer m -> Maybe WalletId -> Maybe AccountIndex -> Maybe (V1 Address) -> WalletLayerResponse m [Transaction]
getTxs pwl = pwl ^. pwlGetTxs

estimateFees :: forall m. PassiveWalletLayer m -> Payment -> WalletLayerResponse m EstimatedFees
estimateFees pwl = pwl ^. pwlEstimateFees

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

