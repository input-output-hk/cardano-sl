module Cardano.Wallet.Kernel.DB.Read (
    -- * Read-only, pure getters
    accountUtxo
  , accountAvailableUtxo
  , accountTotalBalance
  , hdWallets
  ) where

import           Universum

import           Formatting (build, sformat)
import           Formatting.Buildable (Buildable)

import           Pos.Core (Coin)
import           Pos.Txp (Utxo)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId, HdWallets)
import           Cardano.Wallet.Kernel.DB.HdWallet.Read (HdQueryErr)
import qualified Cardano.Wallet.Kernel.DB.Spec.Read as Spec

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

-- | Returns the total balance for this 'HdAccountId'.
accountTotalBalance :: DB -> HdAccountId -> Coin
accountTotalBalance snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountTotalBalance accountId)

-- | Returns the total balance for this 'HdAccountId'.
hdWallets :: DB -> HdWallets
hdWallets snapshot = snapshot ^. dbHdWallets
