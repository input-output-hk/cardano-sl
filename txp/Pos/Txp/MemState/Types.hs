{-# LANGUAGE TemplateHaskell #-}

-- | Type stored in the Txp holder.

module Pos.Txp.MemState.Types
       ( GenericTxpLocalData (..)
       , GenericTxpLocalDataPure
       , TxpLocalData
       , TxpLocalDataPure
       , TransactionProvenance (..)
       , MemPoolModifyReason (..)
       ) where

import           Universum

import           Data.Aeson.TH (defaultOptions, deriveJSON)

import           Pos.Communication.Types.Protocol (PeerId)
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

-- TODO COMMENT
data TransactionProvenance
    = FromPeer PeerId
    | History
    deriving (Show)

$(deriveJSON defaultOptions ''TransactionProvenance)

-- | Enumeration of all reasons for modifying the mempool.
data MemPoolModifyReason =
      -- | Apply a block created by someone else.
      ApplyBlock
      -- | Apply a block created by us.
    | CreateBlock
      -- | Include a transaction. It came from this peer.
    | ProcessTransaction TransactionProvenance
      -- TODO COMMENT
    | Custom Text
      -- TODO COMMENT
    | Unknown
    deriving Show

$(deriveJSON defaultOptions ''MemPoolModifyReason)
