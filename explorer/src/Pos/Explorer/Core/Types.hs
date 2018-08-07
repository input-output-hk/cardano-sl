-- | Module containing explorer-specific datatypes

module Pos.Explorer.Core.Types
       ( TxExtra (..)
       , AddrHistory
       ) where

import           Universum

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Chain.Block (HeaderHash)
import           Pos.Core (Timestamp)
import           Pos.Core.Chrono (NewestFirst)
import           Pos.Core.Txp (TxId, TxUndo)

type AddrHistory = NewestFirst [] TxId

data TxExtra = TxExtra
    { teBlockchainPlace :: !(Maybe (HeaderHash, Word32))
    , teReceivedTime    :: !(Maybe Timestamp)
    -- non-strict on purpose, see comment in `processTxDo` in Pos.Explorer.Txp.Local
    , teInputOutputs    :: TxUndo
    } deriving (Show, Generic, Eq)

deriveSimpleBi ''TxExtra [
    Cons 'TxExtra [
        Field [| teBlockchainPlace :: Maybe (HeaderHash, Word32) |],
        Field [| teReceivedTime    :: Maybe Timestamp            |],
        Field [| teInputOutputs    :: TxUndo                     |]
    ]]
