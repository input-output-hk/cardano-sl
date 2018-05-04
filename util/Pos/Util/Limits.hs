{-# LANGUAGE RankNTypes #-}

-- | This module provides helpers to construct values that have
-- serialization size under the certain limit. Used in block creation
-- and ssc stripping.

module Pos.Util.Limits
       ( spanHashMap
       , stripHashMap
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Serokell.Data.Memory.Units (Byte)

import           Pos.Binary.Class (Bi, biSize)

-- | Spans hash map into two (almost same) parts.
spanHashMap :: (Hashable k, Eq k) => HashMap k v -> (HashMap k v, HashMap k v)
spanHashMap m | HM.null m = (HM.empty, HM.empty)
spanHashMap m = (HM.fromList esl, HM.fromList esr)
  where
    es = HM.toList m
    (esl, esr) = splitAt (length es `div` 2) es

-- | Throws away map elements until map size is leq than limit. Will
-- fail if limit is less than size of empty hashmap (1 byte).
stripHashMap
    :: forall k v.
       (Hashable k, Ord k, Bi k, Bi v)
    => Byte -> HashMap k v -> Maybe (HashMap k v)
stripHashMap lim m'
    | lim < biSize (HM.empty :: HashMap k v) = Nothing
    | otherwise = Just $ takeFromMapDo m' HM.empty
  where
    -- given two maps m and n where size of m is less than
    -- limit it tries to add as much as possible to it from n
    populate m _ | biSize m > lim = error "getLocalPayload@takeFromMap@populate"
    populate m n | HM.null n = m
    populate m n | HM.size n == 1 =
        let merged = m <> n
        in bool m merged (biSize merged <= lim)
    populate m n =
        let (n1,n2) = spanHashMap n
            mn1 = m `HM.union` n1
        in bool (populate m n1) (populate mn1 n2) (biSize mn1 <= lim)

    takeFromMapDo l r =
        if biSize l > lim
        then uncurry takeFromMapDo $ spanHashMap l
        else populate l r
