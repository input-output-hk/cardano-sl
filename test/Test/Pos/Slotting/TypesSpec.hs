{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Slotting.TypesSpec
    ( spec
    ) where

import           Universum

import           Data.Map
import           Data.Time.Units    (Millisecond)
import           Pos.Core           (EpochIndex (..), TimeDiff (..))
import           Pos.Slotting.Types
import           Test.Hspec         (Spec, describe, it, shouldBe)

testEpochSlottingData :: EpochSlottingData
testEpochSlottingData = EpochSlottingData
    { esdSlotDuration = 1000 :: Millisecond
    , esdStartDiff    = 1000 :: TimeDiff
    }

-- stack test --test-arguments "-m "Test.Pos.Slotting.TypesSpec""
spec :: Spec
spec = describe "Types" $ do
    it "should be valid SlottingData" $ do
        let testSlottingDataMap     = fromList [ (EpochIndex 0, testEpochSlottingData)
                                               , (EpochIndex 1, testEpochSlottingData)
                                               ]
        let testSlottingData        = createSlottingDataUnsafe testSlottingDataMap
        let testSlottingDataLength  = length $ getAllEpochIndices $ testSlottingData
        testSlottingDataLength `shouldBe` 2
    -- TODO(KS): I don't know how to catch an `error`, seems we need a `Exception` if we
    -- want observable effects.
    -- it "should be invalid SlottingData" $ do
    --     let testSlottingDataMap     = fromList [(EpochIndex 0, testEpochSlottingData)]
    --     let testSlottingData        = createSlottingDataUnsafe testSlottingDataMap
    --     let testSlottingDataLength  = length $ getAllEpochIndices $ testSlottingData
    --     testSlottingDataLength `shouldThrow` anyErrorCall
