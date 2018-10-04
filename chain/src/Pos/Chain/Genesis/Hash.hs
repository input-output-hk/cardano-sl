{-# LANGUAGE Rank2Types #-}

module Pos.Chain.Genesis.Hash
       ( GenesisHash (..)
       ) where

import           Pos.Crypto.Hashing (Hash)

newtype GenesisHash = GenesisHash { getGenesisHash :: forall a . Hash a }
