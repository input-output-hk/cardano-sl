-- | Type stored in the Txp holder.

module Pos.Txp.MemState.Types
       ( GenericTxpLocalData (..)
       , GenericTxpLocalDataPure
       , TxpLocalData
       , TxpLocalDataPure
       , TxpMetrics (..)
       , ignoreTxpMetrics
       ) where

import           GHC.Base               (Int, IO)
import           Universum
import           Data.Time.Units        (Microsecond)

import           Pos.Core.Types     (HeaderHash)
import           Pos.Txp.Toil.Types (MemPool, UndoMap, UtxoModifier)

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

-- | Memory state of Txp. Generic version.
data GenericTxpLocalData extra = TxpLocalData
    { txpUtxoModifier :: !(TVar UtxoModifier)
    , txpMemPool      :: !(TVar MemPool)
    , txpUndos        :: !(TVar UndoMap)
    , txpTip          :: !(TVar HeaderHash)
    , txpExtra        :: !(TVar extra)
    }

-- | Pure version of GenericTxpLocalData.
type GenericTxpLocalDataPure extra = (UtxoModifier, MemPool, UndoMap, HeaderHash, extra)

-- | Memory state of Txp. This version is used by actual Txp implementation.
type TxpLocalData = GenericTxpLocalData ()

-- | Pure version of TxpLocalData.
type TxpLocalDataPure = GenericTxpLocalDataPure ()

-- | Effectful getters and setters for metrics related to the Txp data.
--   TODO this should not be fixed at IO, if being able to mock features
--   remains a goal. But we can't free it up right now because the current
--   mockable system doesn't work well with ether.
data TxpMetrics = TxpMetrics
    { txpMetricsMemPoolSize :: !(IO Int, Int -> IO ())
      -- | How long is spent trying to modify the mempool.
      --   Reading should give an estimator of the next modify time.
    , txpMetricsModifyTime  :: !(IO Microsecond, Microsecond -> IO ())
    }

-- | A TxpMetrics which always give 0 and never does any writes. Use it if
--   you don't care about metrics.
ignoreTxpMetrics :: TxpMetrics
ignoreTxpMetrics = TxpMetrics
    { txpMetricsMemPoolSize = (pure 0, const (pure ()))
    , txpMetricsModifyTime  = (pure 0, const (pure ()))
    }
