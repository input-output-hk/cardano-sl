-- | Merkle tree-related serialization

module Pos.Binary.Merkle () where

import           Data.Binary.Get  (getWord32be, getWord8)
import           Data.Binary.Put  (putWord32be, putWord8)
import           Universum

import           Pos.Binary.Class (Bi (..))
import           Pos.Merkle       (MerkleNode (..), MerkleRoot (..), MerkleTree (..),
                                   mkBranch, mkLeaf)

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance Bi a => Bi (MerkleNode a) where
    get = do
        tag <- getWord8
        case tag of
            0 -> mkBranch <$> get <*> get
            1 -> mkLeaf <$> get
            _ -> fail ("get@MerkleNode: invalid tag: " ++ show tag)
    put x = case x of
        MerkleBranch{..} -> putWord8 0 >> put mLeft >> put mRight
        MerkleLeaf{..}   -> putWord8 1 >> put mVal

instance Bi a => Bi (MerkleTree a) where
    get = getWord8 >>= \case
        0 -> pure MerkleEmpty
        1 -> MerkleTree <$> getWord32be <*> get
        tag -> fail ("get@MerkleTree: invalid tag: " ++ show tag)
    put MerkleEmpty      = putWord8 0
    put (MerkleTree w n) = putWord8 1 >> putWord32be w >> put n

instance Bi (MerkleRoot a) where
    put (MerkleRoot a) = put a
    get = MerkleRoot <$> get
