-- | Merkle tree-related serialization

module Pos.Binary.Merkle () where

import           Universum

import           Pos.Binary.Class   (Bi (..), Raw, label, putField)
import qualified Pos.Binary.Cbor    as Cbor
import           Pos.Crypto.Hashing (Hash)
import           Pos.Merkle         (MerkleRoot (..), MerkleTree (..), mkMerkleTree, mkMerkleTreeCbor)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance (Bi a, Bi (Hash Raw)) => Bi (MerkleTree a) where
    sizeNPut = putField toList
    get = label "MerkleTree" $ mkMerkleTree <$> get

instance (Cbor.Bi a, Cbor.Bi (Hash Raw)) => Cbor.Bi (MerkleTree a) where
  encode = Cbor.encode . toList
  decode = mkMerkleTreeCbor <$> Cbor.decode

instance (Bi a, Bi (Hash Raw)) => Bi (MerkleRoot a) where
    sizeNPut = putField getMerkleRoot
    get = label "MerkleRoot" $ MerkleRoot <$> get

instance (Cbor.Bi a, Cbor.Bi (Hash Raw)) => Cbor.Bi (MerkleRoot a) where
  encode = Cbor.encode . getMerkleRoot
  decode = MerkleRoot <$> Cbor.decode
