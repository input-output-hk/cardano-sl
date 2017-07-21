{-# LANGUAGE ScopedTypeVariables        #-}

module Test.Pos.Slotting.MemState.HolderSpec
    ( spec
    ) where

import           Universum

import           Data.HashMap.Strict          as HM
import qualified Ether
import           Ether.Reader
import           Test.Hspec                   

import           Data.Time.Units              (Second, Millisecond, convertUnit)
import           Pos.Core.Types               (Timestamp)
import           Pos.Slotting.MemState.Holder
import           Pos.Slotting.Types


singleEpochSlottingData :: IO (TVar SlottingData)
singleEpochSlottingData = newTVarIO $ HM.singleton 1 simpleEpochSlottingData
  where
    simpleEpochSlottingData :: EpochSlottingData 
    simpleEpochSlottingData = EpochSlottingData
        { esdSlotDuration = simpleSlotDuration
        , esdStart        = simpleTimestamp
        } 

    simpleSlotDuration :: Millisecond
    simpleSlotDuration = convertUnit (10 :: Second)

    simpleTimestamp :: Timestamp
    simpleTimestamp = 1000000

testSlottingVar :: IO SlottingVar
testSlottingVar = do
    esd <- singleEpochSlottingData
    pure (100, esd)

instance Ether.Reader.MonadReader SlottingVar SlottingVar IO where
    local  = error "Not being used!"
    ask    = testSlottingVar
    reader = error "Not being used!"

-- stack test --no-run-tests
-- stack test --test-arguments "-m "Test.Pos.Slotting.MemState.HolderSpec""
spec :: Spec
spec = describe "SlottingData spec" $ do
  simpleSlottingDataSpec

simpleSlottingDataSpec :: Spec
simpleSlottingDataSpec = describe "simple SlottingData" $ do
    it "should return the set Timestamp" $ do
      let expected = (100 :: Timestamp)
      found <- askSlottingTimestamp

      expected `shouldBe` found

    it "should return the set SlottingVar" $ do
      let expected = 1
      found <- askSlottingVar >>= readTVarIO

      expected `shouldBe` HM.size found

    -- TODO(ks): Shouldn't be too hard to test `MonadSlotsData` for someone who
    -- understands this transformer stack. I really don't.

