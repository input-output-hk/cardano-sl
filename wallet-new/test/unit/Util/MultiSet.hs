-- | Multisets
--
-- Intended for qualified import.
module Util.MultiSet (
    MultiSet -- opaque
  , empty
  , union
  , singleton
  , size
  , toList
  , findMin
  , medianWithDefault
  ) where

import Universum hiding (empty, toList)

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

singleton :: a -> MultiSet a
singleton a = MultiSet 1 (Map.singleton a 1)

size :: MultiSet a -> Int
size = multiSetSize

toList :: MultiSet a -> [a]
toList = concatMap (uncurry (flip replicate)) . Map.toList . multiSetElems

findMin :: MultiSet a -> a
findMin (MultiSet _ m) = fst (Map.findMin m)

{-------------------------------------------------------------------------------
  Specialized functions
-------------------------------------------------------------------------------}

medianWithDefault :: a -> MultiSet a -> a
medianWithDefault def s =
    case drop (size s `div` 2) (toList s) of
      []  -> def
      a:_ -> a
