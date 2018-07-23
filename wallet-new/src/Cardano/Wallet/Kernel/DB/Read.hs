module Cardano.Wallet.Kernel.DB.Read (
    -- * Read-only, pure getters
    accountUtxo
  , accountAvailableUtxo
  , accountAvailableBalance
  , accountTotalBalance
  , accountAddresses
  , hdWallets
  , lookupAddressMeta
  ) where

import           Universum

import           Control.Lens (to)
import qualified Data.Map.Strict as Map

import           Formatting (build, sformat)
import           Formatting.Buildable (Buildable)

import           Pos.Chain.Txp (Utxo)
import           Pos.Core (Address, Coin)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.BlockMeta (AddressMeta,
                     blockMetaAddressMeta)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdAddress,
                     HdWallets)
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (HdQueryErr,
                     readAddressesByAccountId, readHdAccountCurrentCheckpoint)
import           Cardano.Wallet.Kernel.DB.InDb (fromDb)
import           Cardano.Wallet.Kernel.DB.Spec (Checkpoint, checkpointBlockMeta)
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

-- | Lookup the given 'Address' in the 'HdAccount' current checkpoint.
-- This getter returns a 'Maybe' because in case of freshly-generated
-- addresses (which has to be yet incorporated into a block) this lookup might
-- fail, but upstream consumers might want to deal with this gracefully (for
-- example in case a new prestine account is populated with many addresses and
-- the same account is requested by the web API).
lookupAddressMeta :: DB
                  -> HdAccountId
                  -> Address
                  -> Maybe AddressMeta
lookupAddressMeta snapshot accountId cardanoAddress
    = getAddressMeta $ walletQuery' snapshot checkpoint
    where
        checkpoint = readHdAccountCurrentCheckpoint accountId

        getAddressMeta :: Checkpoint -> Maybe AddressMeta
        getAddressMeta currentCheckpoint =
            currentCheckpoint ^. checkpointBlockMeta
                               . blockMetaAddressMeta
                               . fromDb
                               . to (Map.lookup cardanoAddress)
