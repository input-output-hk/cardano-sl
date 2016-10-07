{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Merkle tree implementation.

module Pos.Merkle
       ( MerkleRoot
       , getMerkleRoot
       ) where

import           Data.Binary   (Binary)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Universum

newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: ()
    } deriving (Show, Eq, Ord, Generic, Binary)

deriveSafeCopySimple 0 'base ''MerkleRoot
