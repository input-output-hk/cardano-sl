-- | Module containing explorer-specific datatypes

module Pos.Explorer.Core.Types
       ( TxExtra (..)
       , AddrHistory
       ) where

import           Universum

import           Pos.Core.Types  (HeaderHash, Timestamp)
import           Pos.Txp.Core    (TxId, TxUndo)
import           Pos.Util.Chrono (NewestFirst)

type AddrHistory = NewestFirst [] TxId

data TxExtra = TxExtra
    { teBlockchainPlace :: !(Maybe (HeaderHash, Word32))
    , teReceivedTime    :: !(Maybe Timestamp)
    -- non-strict on purpose, see comment in `processTxDo` in Pos.Explorer.Txp.Local
    , teInputOutputs    :: TxUndo
    } deriving (Show, Generic, Eq)
