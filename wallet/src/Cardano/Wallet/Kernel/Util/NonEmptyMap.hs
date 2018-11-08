{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | Maps with at least one key.
module Cardano.Wallet.Kernel.Util.NonEmptyMap
    ( NonEmptyMap
    , fromMap
    , toMap
    , singleton
    , toNEList
    , toDescNEList
    , findMin
    , findMax
    ) where

import           Universum

import           Data.Coerce
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE
import qualified Data.Map as Map
import           Data.SafeCopy (SafeCopy (..), contain, safeGet, safePut)

newtype NonEmptyMap k v = NonEmptyMap (Map k v)
 deriving (Eq, Ord, Show, Functor, Foldable, Traversable)

fromMap :: Map k v -> Maybe (NonEmptyMap k v)
fromMap m = if Map.null m then Nothing else Just (coerce m)

toMap :: NonEmptyMap k v -> Map k v
toMap = coerce

toNEList :: NonEmptyMap k v -> NonEmpty (k,v)
toNEList (NonEmptyMap m) = NE.fromList (Map.toList m)

toDescNEList :: NonEmptyMap k v -> NonEmpty (k,v)
toDescNEList (NonEmptyMap m) = NE.fromList (Map.toList m)

instance (Ord k, SafeCopy (Map k v)) => SafeCopy (NonEmptyMap k v) where
    getCopy = contain $ NonEmptyMap <$> safeGet
    putCopy (NonEmptyMap m) = contain $ safePut m

instance Ord k => One (NonEmptyMap k v) where
    type OneItem (NonEmptyMap k v) = (k, v)
    one = uncurry singleton

singleton :: Ord k => k -> v -> NonEmptyMap k v
singleton k v = NonEmptyMap (Map.fromList [(k,v)])

findMin :: NonEmptyMap k v -> (k, v)
findMin = NE.head . toNEList

findMax :: NonEmptyMap k v -> (k, v)
findMax = NE.head . toDescNEList
