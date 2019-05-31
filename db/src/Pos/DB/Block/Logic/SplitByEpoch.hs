{-# LANGUAGE RankNTypes #-}

module Pos.DB.Block.Logic.SplitByEpoch
       ( splitByEpoch

       -- For testing.
       , concatEpochs
       ) where

import           Universum

import qualified Data.List.Extra as List
import           Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE

import           Pos.Core (HasEpochIndex (..))
import           Pos.Core.Chrono (NE, OldestFirst (..))

-- | Split a list of 'OldestFirst NE a' into a list of the same type where the
-- where each chunk only contains 'a' from the same epoch and at the start of
-- epoch is a singleton list.
-- The boolean parameter determines whether the first epoch in the input list
-- should be split or not.
-- For the Original era, given a set of epoch boundary (e) and main blocks (m)
-- they would be split as follows:
--
--    blocks                  splitFirst=True      splitFirst=False
--    mmmmemmm                  m,mmm,e,mmm          mmmm,e,mmm
--    emmmemmm                  e,mmm,e,mmm          e,mmm,e,mmm
--
-- For the OBFT era, where there are no EBBs, main blocks with an epoch subscript
-- would be split as follows:
--
--    blocks                  splitFirst=True      splitFirst=False
--    m1m1m1m2m2m2m           m1,m1m1,m2,m2m2m     m1m1m1,m2,m2m2m
--
splitByEpoch :: HasEpochIndex a => Bool -> OldestFirst NE a -> [OldestFirst NE a]
splitByEpoch splitFirst input =
    processTail $ List.groupOn (view epochIndexL) (NE.toList $ getOldestFirst input)
  where
    processTail :: [[a]] -> [OldestFirst NE a]
    processTail lists =
        if splitFirst
            then concatMap handleEpoch lists
            else case lists of
                    [] -> [] -- Shouldn't happen
                    ([] : xs) -> processTail xs -- Shouldn't happen
                    ((x:xs): ys) -> OldestFirst (x :| xs) : concatMap handleEpoch ys

    handleEpoch :: [a] -> [OldestFirst NE a]
    handleEpoch xs =
        case xs of
            []       -> []
            [x]      -> [OldestFirst $ x :| []]
            (x:y:ys) -> map OldestFirst [x :| [], y :| ys]

-- The reverse of 'splitByEpoch'.
concatEpochs :: [OldestFirst NE a] -> Maybe (OldestFirst NE a)
concatEpochs xs =
    case concatMap (NE.toList . getOldestFirst) xs of
        []     -> Nothing
        (y:ys) -> Just (OldestFirst $ y :| ys)
