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
import           Pos.Types                   (SlotId (..))

-- | 'MonadSlotsData' provides access to data necessary for slotting to work.
class Monad m =>
      MonadSlotsData m where

    getSlottingData :: m SlottingData

    default getSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        m SlottingData
    getSlottingData = lift getSlottingData

instance MonadSlotsData m => MonadSlotsData (ReaderT s m) where
instance MonadSlotsData m => MonadSlotsData (ExceptT s m) where
instance MonadSlotsData m => MonadSlotsData (StateT s m) where
instance MonadSlotsData m => MonadSlotsData (KademliaDHT m) where
instance MonadSlotsData m => MonadSlotsData (PeerStateHolder m) where

-- | Type class providing information about current slot.
class MonadSlotsData m =>
      MonadSlots m where

    getCurrentSlot :: m (Maybe SlotId)

    slottingWorkers :: [m ()]

    default getCurrentSlot :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        m (Maybe SlotId)
    getCurrentSlot = lift getCurrentSlot

    default slottingWorkers :: (MonadTrans t, MonadSlots m', t m' ~ m) =>
        [m ()]
    slottingWorkers = map lift slottingWorkers

instance MonadSlots m => MonadSlots (ReaderT s m) where
instance MonadSlots m => MonadSlots (ExceptT s m) where
instance MonadSlots m => MonadSlots (StateT s m) where
instance MonadSlots m => MonadSlots (KademliaDHT m) where
instance MonadSlots m => MonadSlots (PeerStateHolder m) where
