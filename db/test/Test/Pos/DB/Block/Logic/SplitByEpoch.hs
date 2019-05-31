{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

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

import           Pos.Core (EpochIndex (..), HasEpochIndex (..))
import           Pos.Core.Chrono (NE, OldestFirst (..))
import           Pos.DB.Block (concatEpochs, splitByEpoch)

-- A dummy type on which to run our 'splitByEpoch' tests.
newtype Wrapper
    = Wrapper { _unWrapper :: EpochIndex }
    deriving (Eq, Show)

makeLenses ''Wrapper

instance HasEpochIndex Wrapper where
    epochIndexL = unWrapper

-- -----------------------------------------------------------------------------
-- Generators and helpers

-- For each element of a list of 'OldestFirst NE a' convert each sublist
-- into a list of unique epoch indices.
convertEpochs :: HasEpochIndex a => [OldestFirst NE a] -> [[Word64]]
convertEpochs =
    List.map (map (getEpochIndex . view epochIndexL) . NE.toList . getOldestFirst)

wrapRight :: Word64 -> Either a Wrapper
wrapRight = Right . Wrapper . EpochIndex

genWrapperList :: Int -> Gen (OldestFirst NE Wrapper)
genWrapperList shortestChunk = do
    start <- Gen.word64 $ Range.constant 0 1000
    end <- Gen.word64 $ Range.constant (start + 1) (start + 15)
    OldestFirst . NE.fromList <$> concatMapM genSubList [start .. end]
  where
    genSubList :: Word64 -> Gen [Wrapper]
    genSubList n =
        fmap (Wrapper . EpochIndex) <$> Gen.list (Range.linear shortestChunk 10) (pure n)

withTestCount :: TestLimit -> PropertyT IO () -> Property
withTestCount n =
    H.withTests n . H.property

-- -----------------------------------------------------------------------------
-- Tests

-- Splitting followed by concatenation should produce the original list.
prop_splitByEpoch_preserves_elements :: Property
prop_splitByEpoch_preserves_elements =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 1
        splitFirst <- H.forAll Gen.bool
        concatEpochs (splitByEpoch splitFirst xs) === Just xs

-- If each chunks is converted a list of 'Word64' and then `List.nub' is
-- called on each chunk, then the length of each chunk should be 1.
prop_all_chunks_are_same_epoch :: Property
prop_all_chunks_are_same_epoch =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 1
        H.assert $ List.all (\ys -> length ys == 1)
                    . List.map List.nub
                    $ convertEpochs (splitByEpoch True xs)

-- Test the behavior of the 'splitFirst' parameter to 'splitByEpoch'.
prop_spliting_first_is_longer :: Property
prop_spliting_first_is_longer =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 1
        let splitFirstTrue = splitByEpoch True xs
            splitFirstFalse = splitByEpoch False xs
        case splitFirstFalse of
            [] -> H.failure -- Should never happen
            (h:_) -> do
                let initialSingleton = NE.length (getOldestFirst h) <= 1
                if initialSingleton
                    then length splitFirstTrue === length splitFirstFalse
                    else length splitFirstTrue === 1 + length splitFirstFalse
                H.cover 50 "non-trivial" (not initialSingleton)
                H.cover 25 "trivial" initialSingleton

prop_epoch_are_split :: Property
prop_epoch_are_split =
    withTestCount 1000 $ do
        xs <- H.forAll $ genWrapperList 2
        let epochCount = length . List.nub . map (view epochIndexL) . NE.toList $ getOldestFirst xs
        length (splitByEpoch True xs) === 2 * epochCount

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$(discover)
