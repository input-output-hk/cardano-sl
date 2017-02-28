-- | Module containing explorer-specific datatypes

module Pos.Types.Explorer
       ( TxExtra (..)
       ) where

import           Universum

import           Pos.Txp.Core.Types (TxOut)
import           Pos.Types.Core     (HeaderHash)

data TxExtra = TxExtra
    { teBlockchainPlace :: Maybe (HeaderHash, Word32)
    , teInputOutputs    :: [TxOut]
    } deriving (Show, Generic)
