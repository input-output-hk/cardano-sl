{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE TemplateHaskell #-}

module Pos.Wallet.State.Storage.Tx
       ( TxStorage
       , HasTxStorage (txStorage)

       , mkTxStorage

       , getUtxo
       , getOldestUtxo
       , getTxHistory
       ) where

import           Control.Lens       (makeClassy, to)
import           Data.List          (last)
import           Data.SafeCopy      (base, deriveSafeCopySimple)
import           Universum

import           Pos.SafeCopy       ()
import           Pos.Txp.Core.Types (Tx, Utxo)

data TxStorage = TxStorage
    { -- | History of utxo. Necessary for rollbacking transaction
      -- history while rollbacking blocks. Head of this list
      -- is utxo corresponding to last known block.
      _txUtxoHistory :: ![Utxo]
      -- | Set of unspent transaction outputs formed by applying
      -- txLocalTxs to the head of txUtxoHistory. It is need to check
      -- new transactions and run follow-the-satoshi, for example.
    , _txUtxo        :: !Utxo
      -- | History of transactions related to some of the wallet's
      -- addresses. Newer transactions are the first.
    , _txHistory     :: ![Tx]
    }

makeClassy ''TxStorage
deriveSafeCopySimple 0 'base ''TxStorage

mkTxStorage :: Utxo -> TxStorage
mkTxStorage = flip (TxStorage mempty) mempty

type Query a = forall m x. (HasTxStorage x, MonadReader x m) => m a
--type Update a = forall m x. (HasTxStorage x, MonadState x m) => m a

getUtxo :: Query Utxo
getUtxo = view txUtxo

getOldestUtxo :: Query Utxo
getOldestUtxo = view $ txUtxoHistory . to last

getTxHistory :: Query [Tx]
getTxHistory = view txHistory
