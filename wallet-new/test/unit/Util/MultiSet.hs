-- | Multisets
--
-- Intended for qualified import.
module Util.MultiSet (
    MultiSet -- opaque
  , empty
  , union
  , unions
  , singleton
  , fromList
  , size
  , findMin
  , medianWithDefault
  ) where

import           Universum hiding (empty)

import qualified Data.Map.Strict as Map

{-------------------------------------------------------------------------------
  Basic definitions
-------------------------------------------------------------------------------}

-- | Multiset modelled as a map from elements to counts
data MultiSet a = MultiSet {
      multiSetSize  :: !Int
    , multiSetElems :: !(Map a Int)
    }

empty :: MultiSet a
empty = MultiSet 0 Map.empty

union :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
union (MultiSet sz m) (MultiSet sz' m') =
    MultiSet (sz + sz') (Map.unionWith (+) m m')

unions :: Ord a => [MultiSet a] -> MultiSet a
unions = foldr union empty

singleton :: a -> MultiSet a
singleton a = MultiSet 1 (Map.singleton a 1)

fromList :: Ord a => [a] -> MultiSet a
fromList = unions . map singleton

size :: MultiSet a -> Int
size = multiSetSize

findMin :: MultiSet a -> a
findMin (MultiSet _ m) = fst (Map.findMin m)

{-------------------------------------------------------------------------------
  Specialized functions
-------------------------------------------------------------------------------}

medianWithDefault :: a -> MultiSet a -> a
medianWithDefault def s =
    case drop' (size s `div` 2) (Map.toList (multiSetElems s)) of
      []      -> def
      (a,_):_ -> a
  where
    drop' :: Int -> [(a, Int)] -> [(a, Int)]
    drop' _ []           = []
    drop' n ((a,m) : as) | n == m    = as
                         | n <  m    = (a, m - n) : as
                         | otherwise = drop' (n - m) as
