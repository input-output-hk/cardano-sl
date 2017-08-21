-- | Module containing explorer-specific datatypes

module Pos.Explorer.Core.Types
       ( TxExtra (..)
       , AddrHistory
       ) where

import           Universum

import           Pos.Core.Types  (HeaderHash, Timestamp)
import           Pos.Txp.Core    (TxId, TxOutAux)
import           Pos.Util.Chrono (NewestFirst)

type AddrHistory = NewestFirst [] TxId

data TxExtra = TxExtra
    { teBlockchainPlace :: !(Maybe (HeaderHash, Word32))
    , teReceivedTime    :: !Timestamp
    , teInputOutputs    :: NonEmpty TxOutAux  -- non-strict on purpose, see `makeExtra` in Pos.Txp.Logic.Local
    } deriving (Show, Generic, Eq)
