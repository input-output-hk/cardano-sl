module Cardano.Wallet.Kernel.DB.Read (
    -- | Getters across the entire kernel
    walletIds
    -- | Summarize
  , accountsByRootId
  , addressesByRootId
  , addressesByAccountId
  , pendingByAccount
    -- | Lookups
  , lookupHdRootId
  , lookupHdAccountId
  , lookupHdAddressId
  , lookupCardanoAddress
    -- | Properties of an entire root
  , rootAssuranceLevel
  , rootTotalBalance
    -- | Queries on an account's current checkpoint
  , currentUtxo
  , currentAvailableUtxo
  , currentTotalBalance
  , currentAvailableBalance
  , currentAddressMeta
  , currentTxSlotId
  , currentTxIsPending
  ) where

import           Universum

import           Pos.Chain.Txp (TxId, Utxo)
import           Pos.Core (Address, Coin, SlotId)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta)
import           Cardano.Wallet.Kernel.DB.HdWallet
import qualified Cardano.Wallet.Kernel.DB.HdWallet.Read as HD
import           Cardano.Wallet.Kernel.DB.Spec.Pending (Pending)
import           Cardano.Wallet.Kernel.DB.Util.AcidState
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
  Getters across the entire kernel
-------------------------------------------------------------------------------}

walletIds :: DB -> [WalletId]
walletIds db = map (WalletIdHdRnd . view hdRootId)
             $ IxSet.toList
             $ db ^. dbHdWallets . hdWalletsRoots

{-------------------------------------------------------------------------------
  Lift functions from "Cardano.Wallet.Kernel.DB.Spec.Read"

  NOTE: Right now this may seem pretty pointless. The idea is that in the
  future we might have other kinds of wallets; the various submodules of
  ("Cardano.Wallet.Kernel.DB.SomeWalletType.Read") should then be unified
  here. Right now we just wrap and make no effort to unify the types.
-------------------------------------------------------------------------------}

accountsByRootId :: DB -> HdRootId -> IxSet HdAccount
accountsByRootId = liftNoErrorsHd1 HD.accountsByRootId

-- | All addresses in the given wallet
addressesByRootId :: DB -> HdRootId -> IxSet (Indexed HdAddress)
addressesByRootId = liftNoErrorsHd1 HD.addressesByRootId

-- | All addresses in the given account
addressesByAccountId :: DB -> HdAccountId -> IxSet (Indexed HdAddress)
addressesByAccountId = liftNoErrorsHd1 HD.addressesByAccountId

pendingByAccount :: DB -> Map HdAccountId Pending
pendingByAccount = liftNoErrorsHd0 HD.pendingByAccount

lookupHdRootId :: DB -> HdRootId -> Either UnknownHdRoot HdRoot
lookupHdRootId = liftHd1 HD.lookupHdRootId

lookupHdAccountId :: DB -> HdAccountId -> Either UnknownHdAccount HdAccount
lookupHdAccountId = liftHd1 HD.lookupHdAccountId

lookupHdAddressId :: DB -> HdAddressId -> Either UnknownHdAddress HdAddress
lookupHdAddressId = liftHd1 HD.lookupHdAddressId

lookupCardanoAddress :: DB -> Address -> Either UnknownHdAddress HdAddress
lookupCardanoAddress = liftHd1 HD.lookupCardanoAddress

rootAssuranceLevel :: DB -> HdRootId -> Either UnknownHdRoot AssuranceLevel
rootAssuranceLevel = liftHd1 HD.rootAssuranceLevel

rootTotalBalance :: DB -> HdRootId -> Coin
rootTotalBalance = liftNoErrorsHd1 HD.rootTotalBalance

currentUtxo :: DB -> HdAccountId -> Either UnknownHdAccount Utxo
currentUtxo = liftHd1 HD.currentUtxo

currentAvailableUtxo :: DB -> HdAccountId -> Either UnknownHdAccount Utxo
currentAvailableUtxo = liftHd1 HD.currentAvailableUtxo

currentTotalBalance :: DB -> HdAccountId -> Either UnknownHdAccount Coin
currentTotalBalance = liftHd1 HD.currentTotalBalance

currentAvailableBalance :: DB -> HdAccountId -> Either UnknownHdAccount Coin
currentAvailableBalance = liftHd1 HD.currentAvailableBalance

currentAddressMeta :: DB -> HdAddress -> Either UnknownHdAccount AddressMeta
currentAddressMeta = liftHd1 HD.currentAddressMeta

currentTxSlotId :: DB -> TxId -> HdAccountId -> Either UnknownHdAccount (Maybe SlotId)
currentTxSlotId = liftHd2 HD.currentTxSlotId

currentTxIsPending :: DB -> TxId -> HdAccountId -> Either UnknownHdAccount Bool
currentTxIsPending = liftHd2 HD.currentTxIsPending

{-------------------------------------------------------------------------------
  Internal auxiliary
-------------------------------------------------------------------------------}

liftHd0 :: Query' err HdWallets z -> DB -> Either err z
liftHd0 f = runQuery' f . view dbHdWallets

liftHd1 :: (a -> Query' err HdWallets z) -> DB -> a -> Either err z
liftHd1 f db a = liftHd0 (f a) db

liftHd2 :: (a -> b -> Query' err HdWallets z) -> DB -> a -> b -> Either err z
liftHd2 f db a b = liftHd0 (f a b) db

liftNoErrorsHd0 :: Query' Void HdWallets z -> DB -> z
liftNoErrorsHd0 f = runQueryNoErrors f . view dbHdWallets

liftNoErrorsHd1 :: (a -> Query' Void HdWallets z) -> DB -> a -> z
liftNoErrorsHd1 f db a = liftNoErrorsHd0 (f a) db
