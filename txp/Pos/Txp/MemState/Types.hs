{-# LANGUAGE TemplateHaskell #-}

-- | Type stored in the Txp holder.

module Pos.Txp.MemState.Types
       ( GenericTxpLocalData (..)
       , GenericTxpLocalDataPure
       , TxpLocalData
       , TxpLocalDataPure
       , TransactionProvenance (..)
       , MemPoolModifyReason (..)
       , TxpMetrics (..)
       ) where

import           Data.Aeson.TH      (deriveJSON, defaultOptions)
import           Universum
import           Data.Time.Units        (Microsecond)
import           System.Wlog            (LoggerNameBox)

import           Pos.Core.Types     (HeaderHash)
import           Pos.Communication.Types.Protocol (PeerId)
import           Pos.Txp.Toil.Types (MemPool, UndoMap, UtxoModifier)
import           Serokell.Data.Memory.Units (Byte)

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

data TransactionProvenance = FromPeer PeerId | History
  deriving Show

$(deriveJSON defaultOptions ''TransactionProvenance)

-- | Enumeration of all reasons for modifying the mempool.
data MemPoolModifyReason =
      -- | Apply a block created by someone else.
      ApplyBlock
      -- | Apply a block created by us.
    | CreateBlock
      -- | Include a transaction. It came from this peer.
    | ProcessTransaction TransactionProvenance
    | Custom Text
    | Unknown
    deriving Show

$(deriveJSON defaultOptions ''MemPoolModifyReason)

-- | Effectful getters and setters for metrics related to the Txp data.
--   TODO this should not be fixed at IO, if being able to mock features
--   remains a goal. But we can't free it up right now because the current
--   mockable system doesn't work well with ether.
data TxpMetrics = TxpMetrics
    { -- | Called when a thread begins to wait to modify the mempool.
      --   Parameter is the reason for modifying the mempool.
      txpMetricsWait :: !(String -> LoggerNameBox IO ())
      -- | Called when a thread is granted the lock on the mempool. Parameter
      --   indicates how long it waited.
    , txpMetricsAcquire :: !(Microsecond -> LoggerNameBox IO ())
      -- | Called when a thread is finished modifying the mempool and has
      --   released the lock. Parameters indicates time elapsed since acquiring
      --   the lock, and new mempool size.
    , txpMetricsRelease :: !(Microsecond -> Byte -> LoggerNameBox IO ())
    }
