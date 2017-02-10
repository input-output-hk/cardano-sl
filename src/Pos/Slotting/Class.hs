{-# LANGUAGE TypeFamilies #-}

-- | Type class used for slotting functionality.

module Pos.Slotting.Class
       ( MonadSlotsData (..)
       , MonadSlots (..)
       ) where

import           Control.Monad.Trans         (MonadTrans)
import           Universum

import           Pos.Communication.PeerState (PeerStateHolder)
import           Pos.DHT.Real                (KademliaDHT)
import           Pos.Slotting.Types          (SlottingData)
import           Pos.Types                   (EpochIndex, SlotId (..), Timestamp)

-- | 'MonadSlotsData' provides access to data necessary for slotting to work.
class Monad m =>
      MonadSlotsData m where

    getSlottingData :: m SlottingData

    waitPenultEpochEquals :: EpochIndex -> m ()

    putSlottingData :: SlottingData -> m ()

    default getSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        m SlottingData
    getSlottingData = lift getSlottingData

    default waitPenultEpochEquals :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        EpochIndex -> m ()
    waitPenultEpochEquals = lift . waitPenultEpochEquals

    default putSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        SlottingData -> m ()
    putSlottingData = lift . putSlottingData

instance MonadSlotsData m => MonadSlotsData (ReaderT s m) where
instance MonadSlotsData m => MonadSlotsData (ExceptT s m) where
instance MonadSlotsData m => MonadSlotsData (StateT s m) where
instance MonadSlotsData m => MonadSlotsData (KademliaDHT m) where
instance MonadSlotsData m => MonadSlotsData (PeerStateHolder m) where

-- | Type class providing information about current slot.
class MonadSlotsData m =>
      MonadSlots m where

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

    default getCurrentSlotInaccurate :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m SlotId
    getCurrentSlotInaccurate = lift getCurrentSlotInaccurate

instance MonadSlots m => MonadSlots (ReaderT s m) where
instance MonadSlots m => MonadSlots (ExceptT s m) where
instance MonadSlots m => MonadSlots (StateT s m) where
instance MonadSlots m => MonadSlots (KademliaDHT m) where
instance MonadSlots m => MonadSlots (PeerStateHolder m) where
