{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns        #-}
module Chain
    ( ChainFragment (..)
    , Chain
    , emptyChain
    , fromList
    , ChainMeasure (..)
    , lookupBySlot
    , lookupByIndexFromEnd
    , splitBeforeSlot
    , findIntersection

    , addBlock
    , drop
    , take
    , append
    , length
    , pointOnChain

    , applyChainUpdate
    , applyChainUpdates
    , invReaderStates

    , chainHead
    , chainHeadBlockId
    , chainHeadSlot
    , chainBackwardsFrom

    -- testing
    , reifyChainFragment
    , absChainFragment
    , genChain
    , validChain
    , validChainFragment

    , TestChain (..)
    , prop_addBlock
    , prop_drop
    , prop_take
    , prop_append
    , invChain
    , prop_TestChain

    )
    where

import           Prelude hiding (drop, length, take)

import           Data.FingerTree (FingerTree, Measured (..), ViewL (..), (<|))
import qualified Data.FingerTree as FT
import qualified Data.List as L

import           Test.QuickCheck hiding ((><))

import           Block (Block (..), BlockId, ChainMeasure (..), Point, Slot, blockPoint)
import qualified Chain.Abstract as Chain.Abs
import           Chain.Update (ChainUpdate (..))

--
-- Blockchain fragment data type.
--

newtype ChainFragment = ChainFragment (FingerTree ChainMeasure Block)
  deriving Show

type Chain = ChainFragment

emptyChain :: Chain
emptyChain = ChainFragment FT.empty

fromList :: [Block] -> ChainFragment
fromList = ChainFragment . FT.fromList

lookupBySlot :: ChainFragment -> Slot -> FT.SearchResult ChainMeasure Block
lookupBySlot (ChainFragment t) s =
    FT.search (\vl vr -> maxSlot vl >= s && minSlot vr >= s) t

lookupByIndexFromEnd :: ChainFragment -> Int -> FT.SearchResult ChainMeasure Block
lookupByIndexFromEnd (ChainFragment t) n =
    FT.search (\vl vr -> size vl >= len - n && size vr <= n) t
  where
    len = size (measure t)

splitBeforeSlot :: ChainFragment -> Slot -> (ChainFragment, ChainFragment)
splitBeforeSlot (ChainFragment t) s =
    (\(l, r) -> (ChainFragment l, ChainFragment r))
  $ FT.split (\v -> maxSlot v < s) t

findIntersection :: Chain -> Point -> [Point] -> Maybe (Point, Point)
findIntersection c hpoint points =
    go hpoint (hpoint : points)
    where
    go _ [] = Nothing
    go p (p':ps)
        | pointOnChain c p' = Just (p', p)
        | otherwise         = go p' ps

addBlock :: Block -> Chain -> Chain
addBlock b (ChainFragment ft) = ChainFragment (b <| ft)

prop_addBlock :: Block -> Chain.Abs.Chain -> Bool
prop_addBlock b c =
    b : c == absChainFragment (b `addBlock` reifyChainFragment c)

drop :: Int -> Chain -> Chain
drop n (ChainFragment ft) = ChainFragment $ FT.dropUntil (\v -> size v > n) ft

prop_drop :: Int -> Chain.Abs.Chain -> Bool
prop_drop n c =
    L.drop n c == absChainFragment (drop n $ reifyChainFragment c)

take :: Int -> Chain -> Chain
take n (ChainFragment ft) = ChainFragment $ FT.takeUntil (\v -> size v > n) ft

prop_take :: Int -> Chain.Abs.Chain -> Bool
prop_take n c =
    L.take n c == absChainFragment (take n $ reifyChainFragment c)

append :: [Block] -> Chain -> Chain
append bs (ChainFragment r) = ChainFragment (foldr (<|) r bs)

length :: ChainFragment -> Int
length (ChainFragment ft) = size (measure ft)

pointOnChain :: Chain -> Point -> Bool
pointOnChain (ChainFragment ft) (_, bid) = go ft
    where
    -- recursivelly search the fingertree from the left
    go t = case FT.viewl t of
        EmptyL  -> False
        b :< t' | blockId b == bid -> True
                | otherwise        -> go t'

prop_append :: [Block] -> Chain.Abs.Chain -> Bool
prop_append l r =
    l ++ r == absChainFragment (l `append` reifyChainFragment r)

chainHead :: Chain -> Maybe Block
chainHead (ChainFragment ft) = case FT.viewl ft of
    EmptyL -> Nothing
    b :< _ -> Just b

chainHeadBlockId :: Chain -> BlockId
chainHeadBlockId = maybe 0 blockId . chainHead

chainHeadSlot :: Chain -> Slot
chainHeadSlot = maybe 0 blockSlot . chainHead

-- This is the key operation on chains in this model
applyChainUpdate :: ChainUpdate -> Chain -> Chain
applyChainUpdate (AddBlock b) c = b `Chain.addBlock` c
applyChainUpdate (RollBack p) (ChainFragment c) = ChainFragment $ go c
    where
    go v = case FT.viewl v of
        EmptyL  -> v
        b :< v' | blockPoint b == p -> v'
                | otherwise         -> go v'

applyChainUpdates :: [ChainUpdate] -> Chain -> Chain
applyChainUpdates = flip (foldl (flip applyChainUpdate))

-- like 'Chain.Volatile.invReaderState'
invReaderStates :: Chain -> readerState -> Bool
invReaderStates = undefined

chainBackwardsFrom :: Chain -> BlockId -> Chain
chainBackwardsFrom c bid = go c
    where
    go :: Chain -> Chain
    go c@(ChainFragment ft) = case FT.viewl ft of
        EmptyL   -> c
        b :< ft' | blockId b == bid -> ChainFragment (b <| ft')
                 | otherwise        -> go (ChainFragment ft')

reifyChainFragment :: Chain.Abs.ChainFragment -> ChainFragment
reifyChainFragment = fromList

absChainFragment :: ChainFragment -> Chain.Abs.ChainFragment
absChainFragment (ChainFragment ft) = foldr (:) [] ft

validChain :: Chain -> Bool
validChain = Chain.Abs.validChain . absChainFragment

validChainFragment :: ChainFragment -> Bool
validChainFragment = Chain.Abs.validChainFragment . absChainFragment

genChain :: Int -> Gen Chain
genChain n = reifyChainFragment <$> Chain.Abs.genChain n

newtype TestChain = TestChain Chain
    deriving Show

instance Arbitrary TestChain where
    arbitrary = do
        Positive n <- arbitrary
        TestChain <$> genChain n

prop_TestChain :: TestChain -> Bool
prop_TestChain (TestChain chain) = validChain chain

-- |
-- TODO: like 'Chain.Volatile.invChainState'
invChain :: Chain -> Bool
invChain = undefined
