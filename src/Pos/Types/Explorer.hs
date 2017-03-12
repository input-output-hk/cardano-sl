-- | Module containing explorer-specific datatypes

module Pos.Types.Explorer
       ( TxExtra (..)
       ) where

import           Universum

import           Data.List.NonEmpty (NonEmpty)
import           Pos.Core.Types     (HeaderHash)
import           Pos.Txp.Core.Types (TxOutAux)

data TxExtra = TxExtra
    { teBlockchainPlace :: Maybe (HeaderHash, Word32)
    , teInputOutputs    :: NonEmpty TxOutAux
    } deriving (Show, Generic)
