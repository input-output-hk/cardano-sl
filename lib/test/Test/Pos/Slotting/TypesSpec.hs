{-# LANGUAGE ScopedTypeVariables #-}

module Test.Pos.Slotting.TypesSpec
    ( spec
    ) where

import           Universum

import           Test.Hspec (Spec, describe, it, shouldBe, shouldSatisfy)

import           Data.Map
import           Data.Maybe (isJust, isNothing)
import           Data.Time.Units (Millisecond, Second, convertUnit)

import           Pos.Core (EpochIndex (..), LocalSlotIndex (..), TimeDiff (..), Timestamp (..))
import           Pos.Slotting.Types




----------------------------------------------------------------------------
-- Test data
----------------------------------------------------------------------------

testEpochSlottingData0 :: EpochSlottingData
testEpochSlottingData0 = EpochSlottingData
    { esdSlotDuration = convertUnit (10 :: Second)
    , esdStartDiff    = 1000 * 1000000 :: TimeDiff
    }

testEpochSlottingData1 :: EpochSlottingData
testEpochSlottingData1 = EpochSlottingData
    { esdSlotDuration = 1000 :: Millisecond
    , esdStartDiff    = 1000 * 1000000  :: TimeDiff
    }

testEpochISlottingData0 :: (EpochIndex, EpochSlottingData)
testEpochISlottingData0 = (EpochIndex 0, testEpochSlottingData0)

testEpochISlottingData1 :: (EpochIndex, EpochSlottingData)
testEpochISlottingData1 = (EpochIndex 1, testEpochSlottingData1)

testSlottingDataMap :: Map EpochIndex EpochSlottingData
testSlottingDataMap = fromList [ testEpochISlottingData0
                               , testEpochISlottingData1
                               ]

testSlottingData :: SlottingData
testSlottingData = createInitSlottingData testEpochSlottingData0 testEpochSlottingData1

----------------------------------------------------------------------------
-- Unit tests
----------------------------------------------------------------------------

-- stack test --no-run-tests
-- stack test cardano-sl --fast --test-arguments "-m Test.Pos.Slotting.Types"
spec :: Spec
spec = describe "Types" $ do
    describe "createSlottingDataUnsafe" $
        it "should be valid" $ do
            let sd = createSlottingDataUnsafe testSlottingDataMap
            let testSlottingDataLength  = length . getAllEpochIndices $ sd
            testSlottingDataLength `shouldBe` 2

    describe "isValidSlottingDataMap" $ do
        it "should be valid" $ do
            let sdMap = fromList [ testEpochISlottingData0
                                 , testEpochISlottingData1
                                 ]
            let isValid = isValidSlottingDataMap sdMap
            isValid `shouldBe` True
        it "should be invalid because < 2 EpochSlottingData" $ do
            let sdMap = fromList [ testEpochISlottingData0
                                 ]
            let isValid = isValidSlottingDataMap sdMap
            isValid `shouldBe` False
        it "should be invalid because EpochIndex non-sequential" $ do
            let sdMap = fromList [ testEpochISlottingData0
                                 , testEpochISlottingData0
                                 ]
            let isValid = isValidSlottingDataMap sdMap
            isValid `shouldBe` False

    describe "createInitSlottingData" $
        it "should be valid" $ do
            let testSlottingDataLength = length . getAllEpochIndices $ testSlottingData
            testSlottingDataLength `shouldBe` 2

    describe "getAllEpochIndices" $
        it "should be valid" $ do
            let testSlottingDataLength = length . getAllEpochIndices $ testSlottingData
            testSlottingDataLength `shouldBe` 2

    describe "getNextEpochIndex" $
        it "should be valid" $ do
            let testSlottingDataLength = getNextEpochIndex testSlottingData
            testSlottingDataLength `shouldBe` 1

    describe "getNextEpochSlottingData" $
        it "should be valid" $ do
            let nextEpochSlottingData = getNextEpochSlottingData testSlottingData
            nextEpochSlottingData `shouldBe` testEpochSlottingData1

    describe "getCurrentEpochIndex" $
        it "should be valid" $ do
            let currentEpochIndex = getCurrentEpochIndex testSlottingData
            currentEpochIndex `shouldBe` EpochIndex 0

    describe "getCurrentEpochSlottingData" $
        it "should be valid" $ do
            let currentEpochSlottingData = getCurrentEpochSlottingData testSlottingData
            currentEpochSlottingData `shouldBe` testEpochSlottingData0

    describe "getCurrentEpochSlottingData" $
        it "should be valid" $ do
            let currentEpochSlottingData = getCurrentEpochSlottingData testSlottingData
            currentEpochSlottingData `shouldBe` testEpochSlottingData0

    describe "lookupEpochSlottingData" $ do
        it "should be found" $ do
            let mEpochSlottingData0 = lookupEpochSlottingData 0 testSlottingData
            let mEpochSlottingData1 = lookupEpochSlottingData 1 testSlottingData
            mEpochSlottingData0 `shouldSatisfy` isJust
            mEpochSlottingData1 `shouldSatisfy` isJust

        it "should not be found" $ do
            let mEpochSlottingData2 = lookupEpochSlottingData 2 testSlottingData
            mEpochSlottingData2 `shouldSatisfy` isNothing

    describe "insertEpochSlottingDataUnsafe" $
        it "should be valid" $ do
            let newSD = insertEpochSlottingDataUnsafe 2 testEpochSlottingData0 testSlottingData
            let lengthNewSD = length . getAllEpochIndices $ newSD
            lengthNewSD `shouldBe` 3

    describe "addEpochSlottingData" $
        it "should be valid" $ do
            let newSD = addEpochSlottingData testSlottingData testEpochSlottingData0
            let lengthNewSD = length . getAllEpochIndices $ newSD
            lengthNewSD `shouldBe` 3

    describe "computeSlotStart" $
        it "should be correct" $ do
            -- Friday, July 14, 2017 2:40:00 AM = 1500000000 seconds
            let systemStart = Timestamp $ 1500000000 * 1000000
            let localSlotIndex = UncheckedLocalSlotIndex 3
            let epochSlottingData = testEpochSlottingData0
            let slotStart = computeSlotStart systemStart localSlotIndex epochSlottingData
            -- Slot should start on:
            -- system start + slotNumber * slotDuration (in secs)
            -- epochStartTime = systemStart + esdStartDiff
            --                = 1500000000  + 1000            = 1500001000
            -- slotStartTime  = epochSlotDuration * slotIndex =
            --                = 10                * 3         = 30
            -- 1500000000 + 1000 + 30 = 1500001030
            -- In microseconds = 1500001030 * 10 ^ 6 = 1500001030000000
            slotStart `shouldBe` 1500001030000000
