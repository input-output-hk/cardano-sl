{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

-- | Pos.Slotting specification

module Test.Pos.SlottingSpec
       ( spec
       ) where

import           Control.TimeWarp.Timed  (MonadTimed (currentTime), TimedT, for,
                                          runTimedT, wait)
import           Data.Time.Units         (fromMicroseconds, toMicroseconds)
import           Test.Hspec              (Spec, describe)
import           Test.Hspec.QuickCheck   (prop)
import           Test.QuickCheck         (Property, choose, ioProperty, (===))
import           Test.QuickCheck.Monadic (PropertyM, assert, monadic, pick, run)
import           Universum

import           Pos.Constants           (epochDuration, slotDuration)
import           Pos.Slotting            (MonadSlots (..), Timestamp (..), flattenSlotId,
                                          getCurrentSlot, unflattenSlotId)
import           Pos.Types               (EpochIndex, LocalSlotIndex, SlotId (..))

import           Test.Pos.Util           ()

spec :: Spec
spec = describe "Slotting" $ do
    describe "getCurrentSlot" $ do
        prop (mconcat ["for any (a, b) if one waits for "
                      , "`a * epochDuration + b * slotDuration + k`, "
                      , "where k is less than slotDuration, "
                      , "getCurrentSlot will return `SlotId a b`"
                      ])
            waitForSlot

    describe "flattening" $ do
        prop "unflattening after flattening returns original SlotId"
            flattenThenUnflatten

flattenThenUnflatten :: SlotId -> Property
flattenThenUnflatten si = si === unflattenSlotId (flattenSlotId si)

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
