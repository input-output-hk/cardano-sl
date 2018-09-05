{-# LANGUAGE Rank2Types #-}

module Pos.Core.Configuration.GenesisHash
       ( GenesisHash (..)
       ) where

import           Pos.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }
