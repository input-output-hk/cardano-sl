{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisHash
       ( HasGenesisHash
       , withGenesisHash
       , genesisHash
       ) where

import           Data.Reflection    (Given (..), give)
import           Pos.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }

type HasGenesisHash = Given GenesisHash

withGenesisHash :: (forall a . Hash a) -> (HasGenesisHash => r) -> r
withGenesisHash gh = give (GenesisHash gh)

genesisHash :: HasGenesisHash => Hash a
genesisHash = getGenesisHash given
