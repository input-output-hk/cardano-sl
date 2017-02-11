-- | Merkle tree-related serialization

module Pos.Binary.Merkle () where

import           Data.Binary.Get  (label)
import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Merkle       (MerkleRoot (..), MerkleTree (..), mkMerkleTree)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance Bi a => Bi (MerkleTree a) where
    get = label "MerkleTree" $ mkMerkleTree <$> get
    put = put . toList

instance Bi (MerkleRoot a) where
    put (MerkleRoot a) = put a
    get = label "MerkleRoot" $ MerkleRoot <$> get
