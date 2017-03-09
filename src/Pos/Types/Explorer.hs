-- | Module containing explorer-specific datatypes

module Pos.Types.Explorer
       ( TxExtra (..)
       ) where

import           Universum

import           Pos.Core.Types     (HeaderHash)
import           Pos.Txp.Core.Types (TxOut)

data TxExtra = TxExtra
    { teBlockchainPlace :: Maybe (HeaderHash, Word32)
    , teInputOutputs    :: [TxOut]
    } deriving (Show, Generic)
