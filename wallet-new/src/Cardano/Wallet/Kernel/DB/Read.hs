module Cardano.Wallet.Kernel.DB.Read (
    -- * Read-only, pure getters
    accountUtxo
  , accountAvailableUtxo
  , accountAvailableBalance
  , accountTotalBalance
  , accountAddresses
  , accountIsTxPending
  , accountTxSlot
  , readAddressMeta
  , hdWallets
  , walletIds
  , walletAccounts
  , walletTotalBalance
  ) where

import           Universum

import qualified Data.Map.Strict as Map
import           Formatting (build, sformat)
import           Formatting.Buildable (Buildable)

import           Pos.Chain.Txp (TxId, Utxo)
import           Pos.Core (Address, Coin, SlotId, mkCoin, unsafeAddCoin)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta,
                     blockMetaSlotId, localBlockMeta)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccount, HdAccountId,
                     HdAddress, HdRootId, HdWallets, UnknownHdAccount,
                     hdAccountId, hdRootId, hdWalletsRoots)
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (HdQueryErr,
                     readAccountsByRootId, readAddressesByAccountId,
                     readHdAccountCurrentCheckpoint)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (IsCheckpoint, cpAddressMeta,
                     cpBlockMeta, cpPending)
import qualified Cardano.Wallet.Kernel.DB.Spec.Pending as Pending
import qualified Cardano.Wallet.Kernel.DB.Spec.Read as Spec
import           Cardano.Wallet.Kernel.DB.Util.IxSet (Indexed, IxSet)
import qualified Cardano.Wallet.Kernel.DB.Util.IxSet as IxSet
import           Cardano.Wallet.Kernel.Types (WalletId (..))

{-------------------------------------------------------------------------------
                              Wallet getters

  The @only@ effectful function we expose is 'getWalletSnapshot', which reads
  the full DB 'Snapshot' and returns it.
  All the other getters are completely pure and take the 'Snapshot' as input,
  so that users of the wallet are forced to re-use the same 'Snapshot' in case
  they want to read the state of the wallet multiple times within the same
  code block / handler.

-------------------------------------------------------------------------------}

walletQuery' :: forall e a. (Buildable e)
             => DB
             -> HdQueryErr e a
             -> a
walletQuery' snapshot qry= do
    let res = qry (snapshot ^. dbHdWallets)
    either err identity res
    where
        err = error . sformat build

{-------------------------------------------------------------------------------
  Pure getters on the 'DbSnapshot'.
-------------------------------------------------------------------------------}

-- | Returns the Utxo for the input 'HdAccountId'.
accountUtxo :: DB -> HdAccountId -> Utxo
accountUtxo snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountUtxo accountId)

-- | Returns the available Utxo for the input 'HdAccountId'.
accountAvailableUtxo :: DB -> HdAccountId -> Utxo
accountAvailableUtxo snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountAvailableUtxo accountId)

-- | Returns the available balance for the input 'HdAccountId'.
accountAvailableBalance :: DB -> HdAccountId -> Coin
accountAvailableBalance snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountAvailableBalance accountId)

-- | Returns the total balance for this 'HdAccountId'.
accountTotalBalance :: DB -> HdAccountId -> Coin
accountTotalBalance snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountTotalBalance accountId)

-- | Returns the total balance for this 'HdAccountId'.
accountAddresses :: DB -> HdAccountId -> IxSet (Indexed HdAddress)
accountAddresses snapshot accountId
    = walletQuery' snapshot (readAddressesByAccountId accountId)

-- | Reads the given 'Address' in the 'HdAccount' current checkpoint.
readAddressMeta :: DB -> HdAccountId -> Address -> AddressMeta
readAddressMeta snapshot accountId cardanoAddress
    = view (cpAddressMeta cardanoAddress) (walletQuery' snapshot checkpoint)
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId

-- | All HdWallets
hdWallets :: DB -> HdWallets
hdWallets snapshot = snapshot ^. dbHdWallets

-- | All 'WalletId'
walletIds :: DB -> [WalletId]
walletIds db = map (WalletIdHdRnd . view hdRootId)
             $ IxSet.toList
             $ db ^. dbHdWallets . hdWalletsRoots

-- | All the accounts for a 'HdRoot', given its 'HdRootId'.
walletAccounts :: DB -> HdRootId -> IxSet HdAccount
walletAccounts snapshot rootId
    = walletQuery' snapshot (readAccountsByRootId rootId)

-- | Computes the total balance for this wallet, given its 'HdRootId'.
walletTotalBalance :: DB -> HdRootId -> Coin
walletTotalBalance db rootId =
    IxSet.foldl' (\total account ->
                      total `unsafeAddCoin`
                      accountTotalBalance db (account ^. hdAccountId)
                 )
                 (mkCoin 0)
                 (walletAccounts db rootId)

-- | Reads the current slot of a Tx in the 'HdAccount' current checkpoint.
accountTxSlot :: DB -> HdAccountId -> TxId -> Maybe SlotId
accountTxSlot snapshot accountId txId
    = walletQuery' snapshot (queryTxSlotId txId accountId)

-- | Checks wherher a Tx is pending in the 'HdAccount' current checkpoint.
accountIsTxPending :: DB -> HdAccountId -> TxId -> Bool
accountIsTxPending snapshot accountId txId
    = walletQuery' snapshot (queryTxIsPending txId accountId)

{-------------------------------------------------------------------------------
  Pure functions that support read-only operations on an account Checkpoint.
-------------------------------------------------------------------------------}
txSlot :: IsCheckpoint c => TxId -> c -> (Maybe SlotId)
txSlot txId c = Map.lookup txId slots
  where
    blockMeta = localBlockMeta (c ^. cpBlockMeta)
    slots = view (blockMetaSlotId . fromDb) blockMeta

isTxPending :: IsCheckpoint c => TxId -> c -> Bool
isTxPending txId c = Pending.member txId (c ^. cpPending)

{-------------------------------------------------------------------------------
  Public queries on an account.
-------------------------------------------------------------------------------}
queryTxSlotId :: TxId -> HdAccountId -> HdQueryErr UnknownHdAccount (Maybe SlotId)
queryTxSlotId txId accountId db
    = txSlot txId <$> checkpoint
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId db

queryTxIsPending :: TxId -> HdAccountId -> HdQueryErr UnknownHdAccount Bool
queryTxIsPending txId accountId db
    = isTxPending txId <$> checkpoint
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId db
