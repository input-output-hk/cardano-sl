-- | Slotting functionality.

module Pos.Slotting
       ( Timestamp (..)
       , MonadSlots (..)
       , getCurrentSlot
       ) where

import           Control.TimeWarp.Timed (Microsecond)
import           Universum

import           Pos.Constants          (epochDuration, slotDuration)
import           Pos.Types              (SlotId (..))

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
    f t =
        SlotId
        { siEpoch = fromIntegral $ t `div` epochDuration
        , siSlot = fromIntegral $ t `mod` epochDuration `div` slotDuration
        }
