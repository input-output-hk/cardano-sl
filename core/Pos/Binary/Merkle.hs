-- | Merkle tree-related serialization

module Pos.Binary.Merkle () where

import           Universum

import           Pos.Binary.Class   (Bi (..), Raw, label, putField)
import           Pos.Crypto.Hashing (Hash)
import           Pos.Merkle         (MerkleRoot (..), MerkleTree (..), mkMerkleTree)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance (Bi a, Bi (Hash Raw)) => Bi (MerkleTree a) where
    sizeNPut = putField toList
    get = label "MerkleTree" $ mkMerkleTree <$> get

instance (Bi a, Bi (Hash Raw)) => Bi (MerkleRoot a) where
    sizeNPut = putField getMerkleRoot
    get = label "MerkleRoot" $ MerkleRoot <$> get
