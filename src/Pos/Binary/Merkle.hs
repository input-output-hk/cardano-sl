-- | Merkle tree-related serialization

module Pos.Binary.Merkle where

import           Control.Monad.Fail (fail)
import           Data.Binary        (getWord8, putWord8)
import           Universum

import           Pos.Binary.Class   (Bi (..))
import           Pos.Merkle         (MerkleNode (..), MerkleTree (..), mkBranch, mkLeaf)

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

-- TODO
--instance Bi a => Bi (MerkleTree a) where
instance Bi (MerkleTree a) where
    get = notImplemented
    put = notImplemented
