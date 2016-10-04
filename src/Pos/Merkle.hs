{-# LANGUAGE DeriveGeneric #-}

-- | Merkle tree implementation.

module Pos.Merkle
       ( MerkleRoot
       , getMerkleRoot
       ) where

import           Universum

newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: ()
    } deriving (Show, Eq, Ord, Generic)
