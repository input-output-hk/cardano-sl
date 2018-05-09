{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisHash
       ( HasGenesisHash
       , withGenesisHash
       , GenesisHash (..)
       , genesisHash
       ) where

import           Data.Coerce (coerce)
import           Data.Reflection (Given (..), give)

import           Pos.Binary.Class (Raw)
import           Pos.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }

type HasGenesisHash = Given GenesisHash

withGenesisHash :: (Hash Raw) -> (HasGenesisHash => r) -> r
withGenesisHash gh = give (GenesisHash (coerce gh))

genesisHash :: HasGenesisHash => Hash a
genesisHash = getGenesisHash given
