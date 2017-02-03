{-# LANGUAGE TypeFamilies #-}

-- | Type class used for slotting functionality.

module Pos.Slotting.Class
       ( MonadSlots (..)
       ) where

import           Control.Monad.Except (ExceptT)
import           Control.Monad.Trans  (MonadTrans)
import           Data.Time.Units      (Millisecond)
import           Universum

import           Pos.DHT.Real         (KademliaDHT)
import           Pos.Types            (SlotId (..), Timestamp)

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp
    getCurrentSlot :: m SlotId
    getSlotDuration :: m Millisecond

    default getSystemStartTime
            :: (MonadTrans t, MonadSlots m', t m' ~ m) => m Timestamp
    getSystemStartTime = lift getSystemStartTime

    default getCurrentTime
            :: (MonadTrans t, MonadSlots m', t m' ~ m) => m Timestamp
    getCurrentTime = lift getCurrentTime

    default getCurrentSlot
            :: (MonadTrans t, MonadSlots m', t m' ~ m) => m SlotId
    getCurrentSlot = lift getCurrentSlot

    default getSlotDuration
            :: (MonadTrans t, MonadSlots m', t m' ~ m) => m Millisecond
    getSlotDuration = lift getSlotDuration

instance MonadSlots m => MonadSlots (ReaderT s m) where
instance MonadSlots m => MonadSlots (ExceptT s m) where
instance MonadSlots m => MonadSlots (StateT s m) where
instance MonadSlots m => MonadSlots (KademliaDHT m) where
