{-# LANGUAGE TypeFamilies #-}

-- | Simple implementation of slotting.

module Pos.Slotting.Impl.Simple
       ( SimpleSlottingMode
       , getCurrentSlotSimple
       , getCurrentSlotBlockingSimple
       , getCurrentSlotInaccurateSimple
       , currentTimeSlottingSimple
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

getCurrentSlotSimple :: SimpleSlottingMode m => m (Maybe SlotId)
getCurrentSlotSimple = currentTimeSlottingSimple >>= slotFromTimestamp

getCurrentSlotBlockingSimple :: SimpleSlottingMode m => m SlotId
getCurrentSlotBlockingSimple = do
    penult <- sdPenultEpoch <$> getSlottingData
    getCurrentSlotSimple >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitPenultEpochEquals (penult + 1)
            getCurrentSlotBlockingSimple

getCurrentSlotInaccurateSimple :: SimpleSlottingMode m => m SlotId
getCurrentSlotInaccurateSimple = do
    penult <- sdPenultEpoch <$> getSlottingData
    getCurrentSlotSimple >>= \case
        Just slot -> pure slot
        Nothing -> currentTimeSlottingSimple >>= approxSlotUsingOutdated penult

currentTimeSlottingSimple :: SimpleSlottingMode m => m Timestamp
currentTimeSlottingSimple = Timestamp <$> currentTime
