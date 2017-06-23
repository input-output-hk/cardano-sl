{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Slotting.Impl.Simple
       ( SimpleSlottingMode
       , simpleGetCurrentSlot
       , simpleGetCurrentSlotBlocking
       , simpleGetCurrentSlotInaccurate
       , simpleCurrentTimeSlotting
       ) where

import           Universum

import           Mockable                    (CurrentTime, Mockable, currentTime)
import           NTP.Example                 ()

import           Pos.Core.Types              (SlotId (..), Timestamp (..))
import           Pos.Slotting.Impl.Util      (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (SlottingData (..))

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type SimpleSlottingMode m = (Mockable CurrentTime m, MonadSlotsData m)

----------------------------------------------------------------------------
-- Implementation
----------------------------------------------------------------------------

simpleGetCurrentSlot :: SimpleSlottingMode m => m (Maybe SlotId)
simpleGetCurrentSlot = simpleCurrentTimeSlotting >>= slotFromTimestamp

simpleGetCurrentSlotBlocking :: SimpleSlottingMode m => m SlotId
simpleGetCurrentSlotBlocking = do
    penult <- sdPenultEpoch <$> getSlottingData
    simpleGetCurrentSlot >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitPenultEpochEquals (penult + 1)
            simpleGetCurrentSlotBlocking

simpleGetCurrentSlotInaccurate :: SimpleSlottingMode m => m SlotId
simpleGetCurrentSlotInaccurate = do
    penult <- sdPenultEpoch <$> getSlottingData
    simpleGetCurrentSlot >>= \case
        Just slot -> pure slot
        Nothing -> simpleCurrentTimeSlotting >>= approxSlotUsingOutdated penult

simpleCurrentTimeSlotting :: SimpleSlottingMode m => m Timestamp
simpleCurrentTimeSlotting = Timestamp <$> currentTime
