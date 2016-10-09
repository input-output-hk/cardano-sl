{-# LANGUAGE ViewPatterns #-}

-- | Slotting functionality.

module Pos.Slotting
       ( Timestamp (..)
       , MonadSlots (..)
       , getCurrentSlot
       , flattenSlotId
       , unflattenSlotId
       ) where

import           Control.TimeWarp.Timed (Microsecond)
import           Universum

import           Pos.Constants          (epochSlots, slotDuration)
import           Pos.Types              (FlatSlotId, SlotId (..))

-- | Timestamp is a number which represents some point in time. It is
-- used in MonadSlots and its meaning is up to implementation of this
-- type class. The only necessary knowledge is that difference between
-- timestamps is microsecond. Hence underlying type is Microsecond.
newtype Timestamp = Timestamp
    { getTimestamp :: Microsecond
    } deriving (Show, Num)

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp

-- | Get id of current slot based on MonadSlots.
getCurrentSlot :: MonadSlots m => m SlotId
getCurrentSlot =
    f . getTimestamp <$> ((-) <$> getCurrentTime <*> getSystemStartTime)
  where
    f :: Microsecond -> SlotId
    f t = unflattenSlotId (fromIntegral $ t `div` slotDuration)

-- | Flatten SlotId (which is basically pair of integers) into a single number.
flattenSlotId :: SlotId -> FlatSlotId
flattenSlotId SlotId {..} = fromIntegral siEpoch * epochSlots + fromIntegral siSlot

-- | Construct SlotId from a flattened variant.
-- TODO: what is antonym of word `flatten`?
unflattenSlotId :: FlatSlotId -> SlotId
unflattenSlotId n =
    let (fromIntegral -> siEpoch, fromIntegral -> siSlot) =
            n `divMod` epochSlots
    in SlotId {..}
