{-# LANGUAGE TypeFamilies #-}

-- | Type class used for slotting functionality.

module Pos.Slotting.Class
       ( MonadSlots (..)
       ) where

import           Control.Monad.Trans   (MonadTrans)
import           Universum

import           Pos.Core.Types        (SlotId (..), Timestamp)
import           Pos.Slotting.MemState (MonadSlotsData)


-- | Type class providing information about current slot.
class MonadSlotsData m => MonadSlots m where

    getCurrentSlot :: m (Maybe SlotId)

    -- | Blocking version of 'getCurrentSlot'. This function doesn't
    -- return until current slot is known.
    getCurrentSlotBlocking :: m SlotId

    currentTimeSlotting :: m Timestamp

    slottingWorkers :: [m ()]

    default getCurrentSlot :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m (Maybe SlotId)
    getCurrentSlot = lift getCurrentSlot

    default getCurrentSlotBlocking :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m SlotId
    getCurrentSlotBlocking = lift getCurrentSlotBlocking

    default currentTimeSlotting :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m Timestamp
    currentTimeSlotting = lift currentTimeSlotting

    default slottingWorkers :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        [m ()]
    slottingWorkers = map lift slottingWorkers

instance {-# OVERLAPPABLE #-}
    (MonadSlots m, MonadTrans t, Monad (t m)) =>
        MonadSlots (t m)
