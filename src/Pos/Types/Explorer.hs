-- | Module containing explorer-specific datatypes

module Pos.Types.Explorer
       ( TxExtra (..)
       ) where

import           Universum

import           Data.List.NonEmpty (NonEmpty)
import           Pos.Core.Types     (HeaderHash, Timestamp)
import           Pos.Txp.Core.Types (TxOutAux)

data TxExtra = TxExtra
    { teBlockchainPlace :: !(Maybe (HeaderHash, Word32))
    , teReceivedTime    :: !Timestamp
    , teInputOutputs    :: NonEmpty TxOutAux  -- non-strict on purpose, see `makeExtra` in Pos.Txp.Logic.Local
    } deriving (Show, Generic)
