-- | This module provides helpers to construct values that have
-- serialization size under the certain limit. Used in block creation
-- and ssc stripping.

module Pos.Util.Limits
       ( bisize
       , spanHashMap
       , takeFromMap
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM

import           Pos.Binary.Class    (Bi)
import qualified Pos.Binary.Class    as Bi

bisize :: (Bi a) => a -> Word64
bisize = fromIntegral . length . Bi.encode

-- | Spans hash map into two (almost same) parts.
spanHashMap :: (Hashable k, Eq k) => HashMap k v -> (HashMap k v, HashMap k v)
spanHashMap m | HM.null m = (HM.empty, HM.empty)
spanHashMap m = (HM.fromList esl, HM.fromList esr)
  where
    es = HM.toList m
    (esl, esr) = splitAt (length es `div` 2) es

-- | Throws away map elements until map size is leq than limit.
takeFromMap :: (Hashable k, Eq k, Bi k, Bi v) => Word64 -> HashMap k v -> HashMap k v
takeFromMap lim m' = takeFromMapDo m' HM.empty
  where
    -- given two maps m and n where size of m is less then
    -- limit it tries to add as much as possible to it from n
    populate m n | HM.null n = m
    populate m _ | bisize m > lim = error "getLocalPayload@takeFromMap@populate"
    populate m n =
        let (n1,n2) = spanHashMap n
            mn1 = m `HM.union` n1
        in if bisize mn1 <= lim
           then populate mn1 n2
           else populate m n1

    takeFromMapDo l r =
        if bisize l >= lim
        then uncurry takeFromMapDo $ spanHashMap l
        else populate l r
