{-# LANGUAGE NamedFieldPuns, MultiParamTypeClasses, ViewPatterns #-}
module ChainRep where

import Data.Word
import Data.List (tails, foldl')
import Data.Hashable
import Data.FingerTree as FT
import Data.Monoid

import Control.Applicative

import Test.QuickCheck
import ChainExperiment2 (Slot, Block(..))
import qualified ChainExperiment2 as Chain

--
-- Blockchain fragment data type.
--

newtype ChainFragment = ChainFragment (FingerTree ChainMeasure Block)
  deriving Show

data ChainMeasure = ChainMeasure {
       minSlot :: !Slot,
       maxSlot :: !Slot,
       size    :: !Int
     }
  deriving Show

instance Monoid ChainMeasure where
  mempty = ChainMeasure maxBound minBound 0
  mappend vl vr =
    ChainMeasure (min (minSlot vl) (minSlot vr))
                 (max (maxSlot vl) (maxSlot vr))
                 (size vl + size vr)

instance Measured ChainMeasure Block where
  measure Block{blockSlot} = ChainMeasure blockSlot blockSlot 1

fromList = ChainFragment . FT.fromList

lookupBySlot :: ChainFragment -> Slot -> FT.SearchResult ChainMeasure Block
lookupBySlot (ChainFragment t) s =
    FT.search (\vl vr -> maxSlot vl >= s && minSlot vr >= s) t

lookupByIndexFromEnd :: ChainFragment -> Int -> FT.SearchResult ChainMeasure Block
lookupByIndexFromEnd (ChainFragment t) n =
    FT.search (\vl vr -> size vl >= len - n && size vr <= n) t
  where
    len = size (measure t)

--lookupByPoint :: ChainFragment -> (Slot, BlockId) -> Maybe Block
--lookupByPoint 

splitBeforeSlot :: ChainFragment -> Slot -> (ChainFragment, ChainFragment)
splitBeforeSlot (ChainFragment t) s =
    (\(l, r) -> (ChainFragment l, ChainFragment r))
  $ FT.split (\v -> maxSlot v < s) t


