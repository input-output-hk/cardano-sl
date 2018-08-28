-- | Some utilities for 'Data.Cache.LRU' (from 'lrucache' package).

module Pos.Util.LRU
       ( clearLRU
       , filterLRU
       ) where

import           Universum

import qualified Data.Cache.LRU as LRU

-- | Remove all items from LRU, retaining maxSize property.
clearLRU :: Ord k => LRU.LRU k v -> LRU.LRU k v
clearLRU = LRU.newLRU . LRU.maxSize

-- | Filter LRU cache by given predicate.
filterLRU :: Ord k => (v -> Bool) -> LRU.LRU k v -> LRU.LRU k v
filterLRU predicate lru =
    LRU.fromList (LRU.maxSize lru) . filter (predicate . snd) . LRU.toList $ lru
