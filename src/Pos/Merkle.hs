{-# LANGUAGE DeriveFoldable  #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Merkle tree implementation.

module Pos.Merkle
       ( MerkleRoot
       , getMerkleRoot
       , MerkleTree
       , mtRoot
       , mtSize
       , mkMerkleTree
       ) where

import           Control.Monad.Fail (fail)
import           Data.Binary        (Binary)
import qualified Data.Binary        as Binary (get, getWord8, put, putWord8)
import           Data.Coerce        (coerce)
import           Data.SafeCopy      (base, deriveSafeCopySimple)
import           Universum

import           Data.ByteArray     (ByteArrayAccess, convert)
import           Pos.Crypto         (Hash, hash, hashRaw)
import           Pos.Util           (Raw)

newtype MerkleRoot a = MerkleRoot
    { getMerkleRoot :: Hash Raw
    } deriving (Show, Eq, Ord, Generic, Binary, ByteArrayAccess)

deriveSafeCopySimple 0 'base ''MerkleRoot

-- TODO: This uses SHA256 (i.e. Hash). Bitcoin uses double SHA256 to protect
-- against some attacks that don't exist yet. It'd likely be nice to use
-- SHA3-256 here instead.
data MerkleTree a
    = MerkleNode { mtRoot  :: MerkleRoot a
                 , mtLeft  :: MerkleTree a
                 , mtRight :: MerkleTree a
                 , mtSize  :: Word32 }
    | MerkleLeaf { mtRoot :: MerkleRoot a
                 , mtVal  :: a
                 , mtSize :: Word32 }
    deriving (Eq, Show, Generic)

deriveSafeCopySimple 0 'base ''MerkleTree

instance Binary a => Binary (MerkleTree a) where
    get = do
        tag <- Binary.getWord8
        case tag of
            0 -> do
                mtRoot <- Binary.get
                mtLeft <- Binary.get
                mtRight <- Binary.get
                return MerkleNode{mtSize = mtSize mtLeft + mtSize mtRight, ..}
            1 -> do
                mtRoot <- Binary.get
                mtVal <- Binary.get
                return MerkleLeaf{mtSize = 1, ..}
            _ -> fail "get@MerkleTree: invalid tag"
    put MerkleNode{..} = do
        Binary.putWord8 0
        Binary.put mtRoot >> Binary.put mtLeft >> Binary.put mtRight
    put MerkleLeaf{..} = do
        Binary.putWord8 1
        Binary.put mtRoot >> Binary.put mtVal

instance Foldable MerkleTree where
    foldMap f x = case x of
        MerkleLeaf{mtVal}           -> f mtVal
        MerkleNode{mtLeft, mtRight} ->
            foldMap f mtLeft `mappend` foldMap f mtRight

mkLeaf :: Binary a => a -> MerkleTree a
mkLeaf a =
    MerkleLeaf
    { mtRoot = MerkleRoot $ coerce (hash a)
    , mtVal  = a
    , mtSize = 1
    }

mkNode :: MerkleTree a -> MerkleTree a -> MerkleTree a
mkNode a b =
    MerkleNode
    { mtRoot  = MerkleRoot $ coerce $
                hashRaw (convert (mtRoot a) <> convert (mtRoot b))
    , mtLeft  = a
    , mtRight = b
    , mtSize  = mtSize a + mtSize b
    }

-- | 'Nothing' will be returned only if the list is empty.
mkMerkleTree :: Binary a => [a] -> Maybe (MerkleTree a)
mkMerkleTree [] = Nothing
mkMerkleTree ls = Just (go (length ls) ls)
  where
    go _ [x] = mkLeaf x
    go len xs = mkNode (go i l) (go (len - i) r)
      where
        i = len `div` 2
        (l, r) = splitAt i xs
