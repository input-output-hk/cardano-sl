{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.DB.Block.Logic.SplitByEpoch
  ( tests
  ) where

import           Universum

import qualified Data.List as List
import qualified Data.List.NonEmpty as NE

import           Control.Lens (makeLenses)

import           Hedgehog (Gen, Property, PropertyT, TestLimit, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Core (HasEpochIndex (..), EpochIndex (..))
import           Pos.Core.Chrono (NE, OldestFirst (..))
import           Pos.DB.Block (concatEpochs, splitByEpoch)

-- Need to test the correctness of the 'splitByEpoch' function on types of the
-- shape 'Either GenesisBlock MainBlock' and on '(Block, Undo)'.

-- A dummy type on which to run our 'splitByEpoch' tests.
newtype Wrapper
    = Wrapper { _unWrapper :: EpochIndex }
    deriving (Eq, Show)

makeLenses ''Wrapper

instance HasEpochIndex Wrapper where
    epochIndexL = unWrapper

instance HasEpochIndex (Wrapper, a) where
    epochIndexL = _1 . epochIndexL

-- -----------------------------------------------------------------------------
-- Generators and helpers

-- For each element of a list of 'OldestFirst NE a' convert each sublist
-- into a list of unique epoch indices.
convertEpochs :: HasEpochIndex a => [OldestFirst NE a] -> [[Word64]]
convertEpochs =
    List.map (map (getEpochIndex . view epochIndexL) . NE.toList . getOldestFirst)

wrapLeft :: Word64 -> Either Wrapper a
wrapLeft = Left . Wrapper . EpochIndex

wrapRight :: Word64 -> Either a Wrapper
wrapRight = Right . Wrapper . EpochIndex

-- Generate lists of form:
--    [Right n, Right n, Left (n + 1), Right (n + 1), Right (n + 1)]
-- genEitherList :: Gen (OldestFirst NE [Either Wrapper Wrapper])
genEitherList :: Int -> Gen (OldestFirst NE (Either Wrapper Wrapper))
genEitherList shortestChunk = do
    n <- Gen.word64 $ Range.constant 0 1000
    xs <- (++) <$> genRights n <*> genLeftThenRights (n + 1)
    pure $ OldestFirst (NE.fromList xs)
  where
    genLeftThenRights :: Word64 -> Gen [Either Wrapper Wrapper]
    genLeftThenRights n =
        (wrapLeft n :) <$> genRights n

    genRights :: Word64 -> Gen [Either a Wrapper]
    genRights n =
        fmap wrapRight <$> Gen.list (Range.linear shortestChunk 10) (pure n)

genWrapperList :: Int -> Gen (OldestFirst NE Wrapper)
genWrapperList shortestChunk = do
    start <- Gen.word64 $ Range.constant 0 1000
    end <- Gen.word64 $ Range.constant (start + 1) (start + 15)
    OldestFirst . NE.fromList <$> concatMapM genSubList [start .. end]
  where
    genSubList :: Word64 -> Gen [Wrapper]
    genSubList n =
        fmap (Wrapper . EpochIndex) <$> Gen.list (Range.linear shortestChunk 10) (pure n)

-- withTestCount :: Num a => a -> PropertyT IO () -> Property
withTestCount :: TestLimit -> PropertyT IO () -> Property
withTestCount n =
    H.withTests n . H.property

-- -----------------------------------------------------------------------------
-- Tests

-- Spanning followed by concatenation should produce the original list.
prop_splitByEpoch_preserves_elements_Either :: Property
prop_splitByEpoch_preserves_elements_Either =
    withTestCount 1000 $ do
        xs <- H.forAll $ genEitherList 1
        concatEpochs (splitByEpoch True xs) === Just xs

-- Spanning followed by concatenation should produce the original list.
prop_splitByEpoch_preserves_elements_Wrapper :: Property
prop_splitByEpoch_preserves_elements_Wrapper =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 1
        concatEpochs (splitByEpoch True xs) === Just xs

-- If each chunks is converted a list of 'Word64' and then `List.nub' is
-- called on each chunk, then the length of each chunk should be 1.
prop_all_chunks_are_same_epoch_Either :: Property
prop_all_chunks_are_same_epoch_Either =
    withTestCount 1000 $ do
        xs <- H.forAll $ genEitherList 1
        H.assert $ List.all (\ys -> length ys == 1)
                    . List.map List.nub
                    $ convertEpochs (splitByEpoch True xs)

-- If each chunks is converted a list of 'Word64' and then `List.nub' is
-- called on each chunk, then the length of each chunk should be 1.
prop_all_chunks_are_same_epoch_Wrapper :: Property
prop_all_chunks_are_same_epoch_Wrapper =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 1
        H.assert $ List.all (\ys -> length ys == 1)
                    . List.map List.nub
                    $ convertEpochs (splitByEpoch True xs)

-- Test the behavior of the 'splitFirst' parameter to 'splitByEpoch'.
prop_spliting_first_is_longer_Either :: Property
prop_spliting_first_is_longer_Either =
    withTestCount 1000 $ do
        xs <- H.forAll $ genEitherList 1
        let splitFirstTrue = splitByEpoch True xs
            splitFirstFalse = splitByEpoch False xs
        case splitFirstFalse of
            [] -> H.failure -- Should never happen
            (h:_) -> do
                let trivial = NE.length (getOldestFirst h) <= 1
                H.cover 50 "  non-trivial" $
                    not trivial && length splitFirstTrue == 1 + length splitFirstFalse
                H.cover 25 "  trivial" trivial

-- Test the behavior of the 'splitFirst' parameter to 'splitByEpoch'.
prop_spliting_first_is_longer_Wrapper :: Property
prop_spliting_first_is_longer_Wrapper =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 1
        let splitFirstTrue = splitByEpoch True xs
            splitFirstFalse = splitByEpoch False xs
        case splitFirstFalse of
            [] -> H.failure -- Should never happen
            (h:_) -> do
                let trivial = NE.length (getOldestFirst h) <= 1
                H.cover 50 "  non-trivial" $ not trivial && length splitFirstTrue == 1 + length splitFirstFalse
                H.cover 25 "  trivial" trivial

prop_epoch_are_split_Either :: Property
prop_epoch_are_split_Either =
    withTestCount 1000 $ do
        xs <- H.forAll $ genEitherList 2
        let epochCount = length . List.nub . map (view epochIndexL) . NE.toList $ getOldestFirst xs
        length (splitByEpoch True xs) === 2 * epochCount

prop_epoch_are_split_Wrapper :: Property
prop_epoch_are_split_Wrapper =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 2
        let epochCount = length . List.nub . map (view epochIndexL) . NE.toList $ getOldestFirst xs
        length (splitByEpoch True xs) === 2 * epochCount

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$(discover)
