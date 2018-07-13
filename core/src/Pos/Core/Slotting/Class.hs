{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.Core.Slotting.Class
       ( HasSlottingVar (..)
       , MonadSlotsData
       , MonadSlots (..)
       , SlottingVar
       , cloneSlottingVar
       ) where

import           Universum

import           Control.Monad.Trans (MonadTrans)

import           Pos.Core.Slotting.SlotId (SlotId (..))
import           Pos.Core.Slotting.Timestamp (Timestamp (..))
import           Pos.Core.Slotting.Types (SlottingData)


type MonadSlotsData ctx m =
    ( MonadReader ctx m
    , HasSlottingVar ctx
    , MonadIO m
    )

type SlottingVar = TVar SlottingData

-- | Create a new 'SlottingVar' with the same contents as the given
-- variable has.
cloneSlottingVar :: MonadIO m => SlottingVar -> m SlottingVar
cloneSlottingVar = readTVarIO >=> newTVarIO

-- | System start and slotting data
class HasSlottingVar ctx where
    slottingTimestamp :: Lens' ctx Timestamp
    slottingVar       :: Lens' ctx SlottingVar

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
    (MonadSlots ctx m, MonadTrans t, MonadReader ctx (t m), MonadIO (t m)) =>
      MonadSlots ctx (t m)
  where
    getCurrentSlot           = lift getCurrentSlot
    getCurrentSlotBlocking   = lift getCurrentSlotBlocking
    currentTimeSlotting      = lift currentTimeSlotting
    getCurrentSlotInaccurate = lift getCurrentSlotInaccurate
