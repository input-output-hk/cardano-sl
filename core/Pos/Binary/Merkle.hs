-- | Merkle tree-related serialization

module Pos.Binary.Merkle () where

import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Merkle (MerkleRoot (..), MerkleTree (..), mkMerkleTree)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance (Bi a) => Bi (MerkleTree a) where
    encode = encode . toList
    decode = mkMerkleTree <$> decode

instance (Bi a) => Bi (MerkleRoot a) where
    encode = encode . getMerkleRoot
    decode = MerkleRoot <$> decode
