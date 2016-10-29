{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Merkle tree implementation.
--
-- See <https://tools.ietf.org/html/rfc6962>.
module Pos.Merkle
       ( MerkleRoot
       , getMerkleRoot
       , MerkleTree
       , mtRoot
       , mtSize
       , mkMerkleTree
       ) where

import           Control.Monad.Fail (fail)
import           Data.Binary        (Binary, get, getWord8, put, putWord8)
import qualified Data.Binary        as Binary (encode)
import qualified Data.ByteString    as BS
import           Data.Coerce        (coerce)
import           Data.MessagePack   (MessagePack)
import           Data.SafeCopy      (base, deriveSafeCopySimple)
import           Prelude            (Show (..))
import           Universum          hiding (show)

import           Data.ByteArray     (ByteArrayAccess, convert)
import           Pos.Crypto         (Hash, hashRaw)
import           Pos.Util           (Raw)

-- TODO: This uses SHA256 (i.e. Hash). Bitcoin uses double SHA256 to protect
-- against some attacks that don't exist yet. It'd likely be nice to use
-- SHA3-256 here instead.
newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: Hash Raw
    } deriving (Show, Eq, Ord, Generic, Binary, ByteArrayAccess)

instance MessagePack (MerkleRoot a)

-- This gives a “redundant constraint” warning due to
-- https://github.com/acid-state/safecopy/issues/46.
deriveSafeCopySimple 0 'base ''MerkleRoot

data MerkleTree a = MerkleEmpty | MerkleTree Word32 (MerkleNode a)
    deriving (Eq, Generic, Foldable)

instance Show a => Show (MerkleTree a) where
  show tree = "Merkle tree: " <> show (toList tree)

-- TODO: MessagePack instances can be more efficient.
instance MessagePack a => MessagePack (MerkleTree a)

data MerkleNode a
    = MerkleBranch { mRoot  :: MerkleRoot a
                   , mLeft  :: MerkleNode a
                   , mRight :: MerkleNode a}
    | MerkleLeaf { mRoot :: MerkleRoot a
                 , mVal  :: a}
    deriving (Eq, Show, Generic)

instance MessagePack a => MessagePack (MerkleNode a)

instance Foldable MerkleNode where
    foldMap f x = case x of
        MerkleLeaf{mVal}            -> f mVal
        MerkleBranch{mLeft, mRight} ->
            foldMap f mLeft `mappend` foldMap f mRight

deriveSafeCopySimple 0 'base ''MerkleNode
deriveSafeCopySimple 0 'base ''MerkleTree

-- This instance is both faster and more space-efficient (as confirmed by a
-- benchmark). Hashing turns out to be faster than decoding extra data.
instance Binary a => Binary (MerkleNode a) where
    get = do
        tag <- getWord8
        case tag of
            0 -> mkBranch <$> get <*> get
            1 -> mkLeaf <$> get
            _ -> fail ("get@MerkleNode: invalid tag: " ++ show tag)
    put x = case x of
        MerkleBranch{..} -> putWord8 0 >> put mLeft >> put mRight
        MerkleLeaf{..}   -> putWord8 1 >> put mVal

instance Binary a => Binary (MerkleTree a)

mkLeaf :: Binary a => a -> MerkleNode a
mkLeaf a =
    MerkleLeaf
    { mVal  = a
    , mRoot = MerkleRoot $ coerce $
              hashRaw (BS.singleton 0 <> toS (Binary.encode a))
    }

mkBranch :: MerkleNode a -> MerkleNode a -> MerkleNode a
mkBranch a b =
    MerkleBranch
    { mLeft  = a
    , mRight = b
    , mRoot  = MerkleRoot $ coerce $
               hashRaw $ mconcat [ BS.singleton 1
                                 , convert (mRoot a)
                                 , convert (mRoot b) ]
    }

mkMerkleTree :: Binary a => [a] -> MerkleTree a
mkMerkleTree [] = MerkleEmpty
mkMerkleTree ls = MerkleTree (fromIntegral lsLen) (go lsLen ls)
  where
    lsLen = length ls
    go _  [x] = mkLeaf x
    go len xs = mkBranch (go i l) (go (len - i) r)
      where
        i = powerOfTwo len
        (l, r) = splitAt i xs

mtRoot :: MerkleTree a -> MerkleRoot a
mtRoot MerkleEmpty      = emptyHash
mtRoot (MerkleTree _ x) = mRoot x

emptyHash :: MerkleRoot a
emptyHash = MerkleRoot (hashRaw mempty)

mtSize :: MerkleTree a -> Word32
mtSize MerkleEmpty      = 0
mtSize (MerkleTree s _) = s

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

    -}
    go w = if w .&. (w - 1) == 0 then w else go (w .&. (w - 1))
