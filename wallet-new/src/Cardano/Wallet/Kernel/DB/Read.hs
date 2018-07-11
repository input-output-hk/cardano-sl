module Cardano.Wallet.Kernel.DB.Read (
    -- * Read-only, pure getters
    accountUtxo
  , accountTotalBalance
  ) where

import           Universum

import           Data.Text.Buildable (Buildable)
import           Formatting (build, sformat)

import           Pos.Core (Coin)
import           Pos.Txp (Utxo)

import           Cardano.Wallet.Kernel.DB.AcidState (DB, dbHdWallets)
import           Cardano.Wallet.Kernel.DB.HdWallet (HdAccountId)
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

-- | Returns the total balance for this 'HdAccountId'.
accountTotalBalance :: DB -> HdAccountId -> Coin
accountTotalBalance snapshot accountId
    = walletQuery' snapshot (Spec.queryAccountTotalBalance accountId)
