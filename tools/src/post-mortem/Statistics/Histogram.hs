module Statistics.Histogram
    ( histogram
    ) where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M

import Universum

histogram :: forall a. Ord a => [a] -> Map a Int
histogram = foldl' step M.empty
  where
    step :: Map a Int -> a -> Map a Int
    step m x = M.insertWith (+) x 1 m
