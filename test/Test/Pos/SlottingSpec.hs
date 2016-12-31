-- | Pos.Slotting specification

module Test.Pos.SlottingSpec
       ( spec
       ) where

import           Control.TimeWarp.Timed  (MonadTimed (currentTime), TimedT, for,
                                          runTimedT, wait)
import           Data.Time.Units         (fromMicroseconds, toMicroseconds)
import           Test.Hspec              (Spec, describe, it, shouldBe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Property, choose, ioProperty)
import           Test.QuickCheck.Monadic (PropertyM, assert, monadic, pick, run)
import           Universum

import           Pos.Constants           (epochDuration, slotDuration)
import           Pos.Slotting            (MonadSlots (..), getCurrentSlot)
import           Pos.Types               (EpochIndex, LocalSlotIndex, SlotId (..),
                                          Timestamp (..))

spec :: Spec
spec = describe "Slotting" $ do
    describe "getCurrentSlot" $ do
        it "returns `SlotId 0 0` at the very beginning" $ do
            runTimedT getCurrentSlot >>= (`shouldBe` SlotId 0 0)

        it "returns `SlotId 1 0` after epochDuration" $ do
            slId <- runTimedT (wait (for epochDuration) *> getCurrentSlot)
            slId `shouldBe` SlotId 1 0

        it "returns `SlotId 1 0` after epochDuration \
           \and then `SlotId 1 1` after slotDuration" $ do
            slIds <- runTimedT (
                (,) <$>
                (wait (for epochDuration) *> getCurrentSlot) <*>
                (wait (for slotDuration) *> getCurrentSlot))
            slIds `shouldBe` (SlotId 1 0, SlotId 1 1)

        prop (mconcat ["for any (a, b) if one waits for "
                      , "`a * epochDuration + b * slotDuration + k`, "
                      , "where k is less than slotDuration, "
                      , "getCurrentSlot will return `SlotId a b`"
                      ])
            waitForSlot

waitForSlot :: EpochIndex -> LocalSlotIndex -> Property
waitForSlot epoch = monadic (ioProperty . runTimedT) . waitForSlotScenario epoch

type EmulationMode = TimedT IO
type EmulationProperty = PropertyM EmulationMode

instance MonadSlots EmulationMode where
    getSystemStartTime = pure 0
    getCurrentTime = Timestamp <$> currentTime

waitForSlotScenario :: EpochIndex -> LocalSlotIndex -> EmulationProperty ()
waitForSlotScenario epoch slot = do
    k <- fromMicroseconds <$> pick (choose (0, toMicroseconds slotDuration - 1))
    let delay =
            (fromIntegral epoch) * epochDuration +
            (fromIntegral slot) * slotDuration +
            k
    run $ wait (for delay)
    si <- run getCurrentSlot
    let expectedSi = SlotId {siEpoch = epoch, siSlot = slot}
    assert (si == expectedSi)
