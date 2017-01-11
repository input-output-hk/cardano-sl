{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting functionality.

module Pos.Slotting
       ( MonadSlots (..)
       , getCurrentSlot
       , getCurrentSlotFlat
       , getSlotStart
       , onNewSlot
       , onNewSlot'
       ) where

import           Control.Monad            (void)
import           Control.Monad.Catch      (MonadCatch, catch)
import           Control.Monad.Except     (ExceptT)
import           Data.Time.Units          (Microsecond)
import           Formatting               (build, sformat, shown, (%))
import           Mockable                 (Delay, Fork, Mockable, MonadMockable, delay,
                                           fork)
import           Serokell.Util.Exceptions ()
import           System.Wlog              (WithLogger, logError, logInfo,
                                           modifyLoggerName)
import           Universum

import           Pos.Constants            (slotDuration)
import           Pos.Types                (FlatSlotId, SlotId (..), Timestamp (..),
                                           flattenSlotId, unflattenSlotId)

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp

instance MonadSlots m => MonadSlots (ReaderT s m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

instance MonadSlots m => MonadSlots (ExceptT s m) where
    getSystemStartTime = lift getSystemStartTime
    getCurrentTime = lift getCurrentTime

instance MonadSlots m => MonadSlots (StateT s m) where
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

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlots m => SlotId -> m Timestamp
getSlotStart (flattenSlotId -> slotId) =
    (Timestamp (fromIntegral slotId * slotDuration) +) <$> getSystemStartTime

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses MonadTimed and assumes consistency between
-- MonadSlots and MonadTimed implementations.
onNewSlot
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       )
    => Bool -> (SlotId -> m ()) -> m a
onNewSlot startImmediately action =
    onNewSlotDo Nothing startImmediately actionWithCatch
  where
    -- [CSL-198]: think about exceptions more carefully.
    actionWithCatch s = action s `catch` handler
    handler :: WithLogger m => SomeException -> m ()
    handler = logError . sformat ("Error occurred: "%build)

onNewSlotDo
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       )
    => Maybe SlotId -> Bool -> (SlotId -> m ()) -> m a
onNewSlotDo expectedSlotId startImmediately action = do
    -- here we wait for short intervals to be sure that expected slot
    -- has really started, taking into account possible inaccuracies
    waitUntilPredicate
        (maybe (const True) (<=) expectedSlotId <$> getCurrentSlot)
    curSlot <- getCurrentSlot
    -- fork is necessary because action can take more time than slotDuration
    when startImmediately $ void $ fork $ action curSlot
    Timestamp curTime <- getCurrentTime
    let nextSlot = succ curSlot
    Timestamp nextSlotStart <- getSlotStart nextSlot
    let timeToWait = nextSlotStart - curTime
    when (timeToWait > 0) $
        do modifyLoggerName (<> "slotting") $
               logInfo $
               sformat ("Waiting for "%shown%" before new slot") timeToWait
           delay timeToWait
    onNewSlotDo (Just nextSlot) True action
  where
    waitUntilPredicate predicate =
        unlessM predicate (shortWait >> waitUntilPredicate predicate)
    shortWaitTime = (10 :: Microsecond) `max` (slotDuration `div` 10000)
    shortWait = delay shortWaitTime

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses MonadTimed and assumes consistency between
-- MonadSlots and MonadTimed implementations.
onNewSlot'
    :: (MonadIO m, MonadMockable m, MonadSlots m, MonadCatch m, WithLogger m)
    => Bool -> (SlotId -> m ()) -> m a
onNewSlot' startImmediately action =
    onNewSlotDo' Nothing startImmediately actionWithCatch
  where
    -- [CSL-198]: think about exceptions more carefully.
    actionWithCatch s = action s `catch` handler
    handler :: WithLogger m => SomeException -> m ()
    handler = logError . sformat ("Error occurred: "%build)

onNewSlotDo'
    :: (MonadIO m, MonadMockable m, MonadSlots m, MonadCatch m, WithLogger m)
    => Maybe SlotId -> Bool -> (SlotId -> m ()) -> m a
onNewSlotDo' expectedSlotId startImmediately action = do
    -- here we wait for short intervals to be sure that expected slot
    -- has really started, taking into account possible inaccuracies
    waitUntilPredicate
        (maybe (const True) (<=) expectedSlotId <$> getCurrentSlot)
    curSlot <- getCurrentSlot
    -- fork is necessary because action can take more time than slotDuration
    when startImmediately $ void $ fork $ action curSlot
    Timestamp curTime <- getCurrentTime
    let nextSlot = succ curSlot
    Timestamp nextSlotStart <- getSlotStart nextSlot
    let timeToWait = nextSlotStart - curTime
    when (timeToWait > 0) $
        do modifyLoggerName (<> "slotting") $
               logInfo $
               sformat ("Waiting for "%shown%" before new slot") timeToWait
           delay timeToWait
    onNewSlotDo' (Just nextSlot) True action
  where
    waitUntilPredicate predicate =
        unlessM predicate (shortWait >> waitUntilPredicate predicate)
    shortWaitTime = (10 :: Microsecond) `max` (slotDuration `div` 10000)
    shortWait = delay shortWaitTime
