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

    , getAddresses
    , applyBlocks
    , rollbackBlocks
    ) where

import           Universum

import           Control.Lens (makeLenses)

import           Cardano.Wallet.API.V1.Types (Account, AccountIndex, AccountUpdate, Address,
                                              NewAccount, NewWallet, Wallet, WalletId, WalletUpdate)

import           Pos.Util.Chrono (NE, OldestFirst (..), NewestFirst (..))
import           Pos.Block.Types (Blund)

------------------------------------------------------------
-- Passive wallet layer
------------------------------------------------------------

-- | The passive wallet (data) layer. See @PassiveWallet@.
data PassiveWalletLayer m = PassiveWalletLayer
    {
    -- * wallets
      _pwlCreateWallet   :: NewWallet -> m Wallet
    , _pwlGetWalletIds   :: m [WalletId]
    , _pwlGetWallet      :: WalletId -> m (Maybe Wallet)
    , _pwlUpdateWallet   :: WalletId -> WalletUpdate -> m Wallet
    , _pwlDeleteWallet   :: WalletId -> m Bool
    -- * accounts
    , _pwlCreateAccount  :: WalletId -> NewAccount -> m Account
    , _pwlGetAccounts    :: WalletId -> m [Account]
    , _pwlGetAccount     :: WalletId -> AccountIndex -> m (Maybe Account)
    , _pwlUpdateAccount  :: WalletId -> AccountIndex -> AccountUpdate -> m Account
    , _pwlDeleteAccount  :: WalletId -> AccountIndex -> m Bool
    -- * addresses
    , _pwlGetAddresses   :: WalletId -> m [Address]
    -- * core API
    , _pwlApplyBlocks    :: OldestFirst NE Blund -> m ()
    , _pwlRollbackBlocks :: NewestFirst NE Blund -> m ()
    }

makeLenses ''PassiveWalletLayer

------------------------------------------------------------
-- Passive wallet layer getters
------------------------------------------------------------

createWallet :: forall m. PassiveWalletLayer m -> NewWallet -> m Wallet
createWallet pwl = pwl ^. pwlCreateWallet

getWalletIds :: forall m. PassiveWalletLayer m -> m [WalletId]
getWalletIds pwl = pwl ^. pwlGetWalletIds

getWallet :: forall m. PassiveWalletLayer m -> WalletId -> m (Maybe Wallet)
getWallet pwl = pwl ^. pwlGetWallet

updateWallet :: forall m. PassiveWalletLayer m -> WalletId -> WalletUpdate -> m Wallet
updateWallet pwl = pwl ^. pwlUpdateWallet

deleteWallet :: forall m. PassiveWalletLayer m -> WalletId -> m Bool
deleteWallet pwl = pwl ^. pwlDeleteWallet


createAccount :: forall m. PassiveWalletLayer m -> WalletId -> NewAccount -> m Account
createAccount pwl = pwl ^. pwlCreateAccount

getAccounts :: forall m. PassiveWalletLayer m -> WalletId -> m [Account]
getAccounts pwl = pwl ^. pwlGetAccounts

getAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> m (Maybe Account)
getAccount pwl = pwl ^. pwlGetAccount

updateAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> AccountUpdate -> m Account
updateAccount pwl = pwl ^. pwlUpdateAccount

deleteAccount :: forall m. PassiveWalletLayer m -> WalletId -> AccountIndex -> m Bool
deleteAccount pwl = pwl ^. pwlDeleteAccount


getAddresses :: forall m. PassiveWalletLayer m -> WalletId -> m [Address]
getAddresses pwl = pwl ^. pwlGetAddresses


applyBlocks :: forall m. PassiveWalletLayer m -> OldestFirst NE Blund -> m ()
applyBlocks pwl = pwl ^. pwlApplyBlocks

rollbackBlocks :: forall m. PassiveWalletLayer m -> NewestFirst NE Blund -> m ()
rollbackBlocks pwl = pwl ^. pwlRollbackBlocks

------------------------------------------------------------
-- Active wallet layer
------------------------------------------------------------

-- An active wallet layer. See @ActiveWallet@.
data ActiveWalletLayer m = ActiveWalletLayer {
      -- | The underlying passive wallet layer
      walletPassiveLayer :: PassiveWalletLayer m
    }
