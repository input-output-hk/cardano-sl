module Util (
    -- * Lists
    at
    -- * Maps and sets
  , disjoint
  , withoutKeys
  , restrictKeys
    -- * Dealing with OldestFirst/NewestFirst
  , liftOldestFirst
  , liftNewestFirst
  ) where

import           Universum

import qualified Data.Map as Map
import qualified Data.Set as Set
import           Pos.Util.Chrono

{-------------------------------------------------------------------------------
  Lists
-------------------------------------------------------------------------------}

-- | Safe version of (!!)
at :: [a] -> Int -> Maybe a
at []     _ = Nothing
at (x:_)  0 = Just x
at (_:xs) i = at xs (i - 1)

{-------------------------------------------------------------------------------
  Maps and sets
-------------------------------------------------------------------------------}

-- | Check that two sets are disjoint
--
-- This is available out of the box from containters >= 0.5.11
disjoint :: Ord a => Set a -> Set a -> Bool
disjoint a b = Set.null (a `Set.intersection` b)

withoutKeys :: Ord k => Map k a -> Set k -> Map k a
m `withoutKeys` s = m `Map.difference` Map.fromSet (const ()) s

restrictKeys :: Ord k => Map k a -> Set k -> Map k a
m `restrictKeys` s = m `Map.intersection` Map.fromSet (const ()) s

{-------------------------------------------------------------------------------
  Dealing with OldestFirst/NewestFirst
-------------------------------------------------------------------------------}

liftOldestFirst :: (f a -> f a) -> OldestFirst f a -> OldestFirst f a
liftOldestFirst f = OldestFirst . f . getOldestFirst

liftNewestFirst :: (f a -> f a) -> NewestFirst f a -> NewestFirst f a
liftNewestFirst f = NewestFirst . f . getNewestFirst
