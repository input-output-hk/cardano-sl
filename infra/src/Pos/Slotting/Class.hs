{-# LANGUAGE TypeFamilies #-}

-- | Type class used for slotting functionality.

module Pos.Slotting.Class
       ( MonadSlots (..)
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)

import           Pos.Core.Slotting (SlotId (..), Timestamp)
import           Pos.Slotting.MemState (MonadSlotsData)


-- | Type class providing information about current slot.
class (MonadSlotsData ctx m) => MonadSlots ctx m where

    getCurrentSlot :: m (Maybe SlotId)

    -- | Blocking version of 'getCurrentSlot'. This function doesn't
    -- return until current slot is known.
    getCurrentSlotBlocking :: m SlotId

    -- | This function tries to predict current slot as accurately as it can.
    -- If 'getCurrentTime' returns unreliable time,
    -- then function returns last known slot
    -- If our slotting data into DB is outdated,
    -- then function tries to extrapolate slot using last know slotting data
    getCurrentSlotInaccurate :: m SlotId

    currentTimeSlotting :: m Timestamp


instance {-# OVERLAPPABLE #-}
    (MonadSlots ctx m, MonadTrans t, MonadReader ctx (t m), MonadIO (t m), Monad (t m)) =>
      MonadSlots ctx (t m)
  where
    getCurrentSlot           = lift getCurrentSlot
    getCurrentSlotBlocking   = lift getCurrentSlotBlocking
    currentTimeSlotting      = lift currentTimeSlotting
    getCurrentSlotInaccurate = lift getCurrentSlotInaccurate
