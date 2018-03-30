-- | Type stored in the Txp holder.

module Pos.Txp.MemState.Types
       ( GenericTxpLocalData (..)
       , TxpLocalData
       ) where

import           Universum

import           Pos.Core.Common (HeaderHash)
import           Pos.Txp.Toil.Types (MemPool, UndoMap, UtxoModifier)

-- | LocalData of transactions processing.
-- There are two invariants which must hold for local data
-- (where um is UtxoModifier, memPool is MemPool and tip is HeaderHash):
-- 1. Suppose 'blks' is sequence of blocks from the very beginning up
-- to 'tip'. If one applies 'blks' to genesis Utxo, resulting Utxo
-- (let's call it 'utxo1') will be such that all transactions from
-- 'memPool' are valid with respect to it.
-- 2. If one applies all transactions from 'memPool' to 'utxo1',
-- resulting 'UtxoModifier' will be equivalent to 'um'.

-- | Memory state of Txp. Generic version.
data GenericTxpLocalData extra = TxpLocalData
    { txpUtxoModifier :: !(TVar UtxoModifier)
    , txpMemPool      :: !(TVar MemPool)
    , txpUndos        :: !(TVar UndoMap)
    , txpTip          :: !(TVar HeaderHash)
    , txpExtra        :: !(TVar extra)
    }

-- | Memory state of Txp. This version is used by actual Txp implementation.
type TxpLocalData = GenericTxpLocalData ()
