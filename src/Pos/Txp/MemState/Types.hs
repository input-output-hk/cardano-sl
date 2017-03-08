-- | Type stored in the Txp holder.

module Pos.Txp.MemState.Types
       ( TxpLocalData (..)
       , TxpLocalDataPure
       ) where

import qualified Control.Concurrent.STM as STM

import           Pos.Core.Types         (HeaderHash)
import           Pos.Txp.Toil.Types     (MemPool, UndoMap, UtxoModifier)

-- | LocalData of transactions processing.
-- There are two invariants which must hold for local data
-- (where um is UtxoModifier, memPool is MemPool and tip is HeaderHash):
-- 1. Suppose 'blks' is sequence of blocks from the very beginning up
-- to 'tip'. If one applies 'blks' to genesis Utxo, resulting Utxo
-- (let's call it 'utxo1') will be such that all transactions from
-- 'memPool' are valid with respect to it.
-- 2. If one applies all transactions from 'memPool' to 'utxo1',
-- resulting Utxo will be equivalent to 'um' with respect to
-- MonadUtxo.

-- | Real data inside TxpHolder.
data TxpLocalData = TxpLocalData
    { txpUtxoModifier :: !(STM.TVar UtxoModifier)
    , txpMemPool      :: !(STM.TVar MemPool)
    , txpUndos        :: !(STM.TVar UndoMap)
    , txpTip          :: !(STM.TVar HeaderHash)
    }

-- | Pure version of TxpLocalData.
type TxpLocalDataPure = (UtxoModifier,  MemPool, UndoMap, HeaderHash)
