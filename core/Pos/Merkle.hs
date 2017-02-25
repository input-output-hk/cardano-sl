{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Merkle tree implementation.
--
-- See <https://tools.ietf.org/html/rfc6962>.
module Pos.Merkle
       ( MerkleRoot(..)
       , MerkleTree (..)
       , mtRoot
       , mkMerkleTree

       , MerkleNode (..)
       , mkBranch
       , mkLeaf
       ) where

import           Data.ByteArray       (ByteArrayAccess, convert)
import qualified Data.ByteString.Lazy as BL (toStrict)
import           Data.Coerce          (coerce)
import qualified Data.Foldable        as Foldable
import           Data.SafeCopy        (SafeCopy (..))
import           Prelude              (Show (..))
import           Universum            hiding (show)

import           Pos.Binary.Class     (Bi, Raw, encode, getCopyBi, putCopyBi)
import           Pos.Crypto           (Hash, hashRaw)

-- | Data type for root of merkle tree.
newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: Hash Raw  -- ^ returns root 'Hash' of Merkle Tree
    } deriving (Show, Eq, Ord, Generic, ByteArrayAccess, Typeable, NFData)

instance Bi (MerkleRoot a) => SafeCopy (MerkleRoot a) where
    getCopy = getCopyBi "MerkleRoot"
    putCopy = putCopyBi

-- | Straightforward merkle tree representation in Haskell.
data MerkleTree a = MerkleEmpty | MerkleTree Word32 (MerkleNode a)
    deriving (Eq, Generic)

instance NFData a => NFData (MerkleTree a)

instance Foldable MerkleTree where
    foldMap _ MerkleEmpty      = mempty
    foldMap f (MerkleTree _ n) = foldMap f n

    null MerkleEmpty = True
    null _           = False

    length MerkleEmpty      = 0
    length (MerkleTree s _) = fromIntegral s

instance Show a => Show (MerkleTree a) where
  show tree = "Merkle tree: " <> show (toList tree)

data MerkleNode a
    = MerkleBranch { mRoot  :: MerkleRoot a
                   , mLeft  :: MerkleNode a
                   , mRight :: MerkleNode a}
    | MerkleLeaf { mRoot :: MerkleRoot a
                 , mVal  :: a}
    deriving (Eq, Show, Generic)

instance NFData a => NFData (MerkleNode a)

instance Foldable MerkleNode where
    foldMap f x = case x of
        MerkleLeaf{mVal}            -> f mVal
        MerkleBranch{mLeft, mRight} ->
            foldMap f mLeft `mappend` foldMap f mRight

instance Bi (MerkleNode a) => SafeCopy (MerkleNode a) where
    getCopy = getCopyBi "MerkleNode"
    putCopy = putCopyBi

instance Bi (MerkleTree a) => SafeCopy (MerkleTree a) where
    getCopy = getCopyBi "MerkleTree"
    putCopy = putCopyBi

mkLeaf :: Bi a => a -> MerkleNode a
mkLeaf a =
    MerkleLeaf
    { mVal  = a
    , mRoot = MerkleRoot $ coerce $
              hashRaw (one 0 <> BL.toStrict (encode a))
    }

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch a b =
    MerkleBranch
    { mLeft  = a
    , mRight = b
    , mRoot  = MerkleRoot $ coerce $
               hashRaw $ mconcat [ one 1
                                 , convert (mRoot a)
                                 , convert (mRoot b) ]
    }

-- | Smart constructor for 'MerkleTree'.
mkMerkleTree :: Bi a => [a] -> MerkleTree a
mkMerkleTree [] = MerkleEmpty
mkMerkleTree ls = MerkleTree (fromIntegral lsLen) (go lsLen ls)
  where
    lsLen = length ls
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

-- | Returns root of merkle tree.
mtRoot :: MerkleTree a -> MerkleRoot a
mtRoot MerkleEmpty      = emptyHash
mtRoot (MerkleTree _ x) = mRoot x

emptyHash :: MerkleRoot a
emptyHash = MerkleRoot (hashRaw mempty)

-- | Return the largest power of two such that it's smaller than X.
--
-- >>> powerOfTwo 64
-- 32
-- >>> powerOfTwo 65
-- 64
powerOfTwo :: (Bits a, Num a) => a -> a
powerOfTwo n
    | n .&. (n - 1) == 0 = n `shiftR` 1
    | otherwise = go n
 where
    {- “x .&. (x - 1)” clears the least significant bit:

           ↓
       01101000     x
       01100111     x - 1
       --------
       01100000     x .&. (x - 1)

       I could've used something like “until (\x -> x*2 > w) (*2) 1”,
       but bit tricks are fun. -}
    go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))
