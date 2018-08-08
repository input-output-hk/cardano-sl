module Cardano.Wallet.Kernel.DB.Read (
    -- * Read-only, pure getters
    accountUtxo
  , accountAvailableUtxo
  , accountAvailableBalance
  , accountTotalBalance
  , accountAddresses
  , hdWallets
  , readAddressMeta
  , walletAccounts
  ) where

import           Universum

import           Formatting (build, sformat)
import           Formatting.Buildable (Buildable)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Address, Coin)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccount, HdAccountId,
                     HdAddress, HdRootId, HdWallets)
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (HdQueryErr,
                     readAccountsByRootId, readAddressesByAccountId,
                     readHdAccountCurrentCheckpoint)
import           Cardano.Wallet.Kernel.DB.Spec (cpAddressMeta)

import qualified Cardano.Wallet.Kernel.DB.Spec.Read as Spec
import           Cardano.Wallet.Kernel.DB.Util.IxSet (IxSet)

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
accountAddresses :: DB -> HdAccountId -> IxSet HdAddress
accountAddresses snapshot accountId
    = walletQuery' snapshot (readAddressesByAccountId accountId)

-- | Returns the total balance for this 'HdAccountId'.
hdWallets :: DB -> HdWallets
hdWallets snapshot = snapshot ^. dbHdWallets

-- | Reads the given 'Address' in the 'HdAccount' current checkpoint.
readAddressMeta :: DB -> HdAccountId -> Address -> AddressMeta
readAddressMeta snapshot accountId cardanoAddress
    = view (cpAddressMeta cardanoAddress) (walletQuery' snapshot checkpoint)
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId

-- | Returns all the accounts for a 'HdRoot', given its 'HdRootId'.
walletAccounts :: DB -> HdRootId -> IxSet HdAccount
walletAccounts snapshot rootId
    = walletQuery' snapshot (readAccountsByRootId rootId)
