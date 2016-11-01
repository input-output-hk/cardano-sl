{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting functionality.

module Pos.Slotting
       (MonadSlots (..)
       , getCurrentSlot
       , getCurrentSlotFlat
       , onNewSlot
       ) where

import           Control.Monad.Catch      (MonadCatch, catch)
import           Control.TimeWarp.Logging (WithNamedLogger, logError)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed, for, fork_, wait)
import           Formatting               (build, sformat, (%))
import           Serokell.Util.Exceptions ()
import           Universum                hiding (catch)

import           Pos.Constants            (slotDuration)
import           Pos.DHT                  (DHTResponseT)
import           Pos.Types                (FlatSlotId, SlotId (..), Timestamp (..),
                                           flattenSlotId, unflattenSlotId)

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp

instance MonadSlots m => MonadSlots (DHTResponseT m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

-- | Get id of current slot based on MonadSlots.
getCurrentSlot :: MonadSlots m => m SlotId
getCurrentSlot =
    f . getTimestamp <$> ((-) <$> getCurrentTime <*> getSystemStartTime)
  where
    f :: Microsecond -> SlotId
    f t = unflattenSlotId (fromIntegral $ t `div` slotDuration)

-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m FlatSlotId
getCurrentSlotFlat = flattenSlotId <$> getCurrentSlot

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses MonadTimed and assumes consistency between
-- MonadSlots and MonadTimed implementations.
onNewSlot
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithNamedLogger m)
    => Bool -> (SlotId -> m ()) -> m a
onNewSlot = onNewSlotDo Nothing

onNewSlotDo
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithNamedLogger m)
    => Maybe SlotId -> Bool -> (SlotId -> m ()) -> m a
onNewSlotDo expectedSlotId startImmediately action = do
    waitUntilPredicate
        (maybe (const True) (<=) expectedSlotId <$> getCurrentSlot)
    curSlot <- getCurrentSlot
    -- fork is necessary because action can take more time than slotDuration
    when startImmediately $ fork_ $ actionWithCatch curSlot
    Timestamp curTime <- getCurrentTime
    let timeToWait = slotDuration - curTime `mod` slotDuration
    wait $ for timeToWait
    onNewSlotDo (Just $ succ curSlot) True actionWithCatch
  where
    waitUntilPredicate predicate =
        unlessM predicate (shortWait >> waitUntilPredicate predicate)
    shortWaitTime = (10 :: Microsecond) `max` (slotDuration `div` 10000)
    shortWait = wait $ for shortWaitTime
    -- TODO: think about exceptions more carefully.
    actionWithCatch s = action s `catch` handler
    handler :: (MonadIO m, WithNamedLogger m) => SomeException -> m ()
    handler = logError . sformat ("Error occurred: "%build)
