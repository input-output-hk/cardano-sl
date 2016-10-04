{-# LANGUAGE DeriveGeneric #-}

-- | Merkle tree implementation.

module Pos.Merkle
       ( MerkleRoot
       , getMerkleRoot
       ) where

import           Data.Binary (Binary)
import           Universum

newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: ()
    } deriving (Show, Eq, Ord, Generic, Binary)
