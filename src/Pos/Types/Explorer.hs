-- | Module containing explorer-specific datatypes

module Pos.Types.Explorer
       ( TxExtra (..)
       ) where

import           Universum

import           Pos.Types.Core (Address, HeaderHash)

data TxExtra = TxExtra
    { teBlockchainPlace :: Maybe (HeaderHash, Word32)
    , teInputAddresses  :: [Address]
    } deriving (Show, Generic)
