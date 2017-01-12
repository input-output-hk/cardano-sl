{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Slotting functionality.

module Pos.Slotting
       ( MonadSlots (..)
       , getCurrentSlotFlat
       , getSlotStart
       , getCurrentSlotUsingNtp

       , onNewSlot
       , onNewSlotWithLogging
       ) where

import           Control.Monad.Catch      (MonadCatch, catch)
import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Trans      (MonadTrans)
import           Control.TimeWarp.Timed   (Microsecond, MonadTimed (..), for, fork_, wait)
import           Formatting               (build, sformat, shown, (%))
import           Serokell.Util.Exceptions ()
import           System.Wlog              (WithLogger, logDebug, logError,
                                           modifyLoggerName)
import           Universum

import           Pos.Constants            (ntpMaxError, ntpPollDelay, slotDuration)
import           Pos.DHT.Model            (DHTResponseT)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Types                (FlatSlotId, SlotId (..), Timestamp (..),
                                           flattenSlotId, unflattenSlotId)

-- | Type class providing information about time when system started
-- functioning.
class Monad m => MonadSlots m where
    getSystemStartTime :: m Timestamp
    getCurrentTime :: m Timestamp
    getCurrentSlot :: m SlotId

    default getSystemStartTime :: (MonadTrans t, MonadSlots m', t m' ~ m) => m Timestamp
    getSystemStartTime = lift getSystemStartTime

    default getCurrentTime :: (MonadTrans t, MonadSlots m', t m' ~ m) => m Timestamp
    getCurrentTime = lift getCurrentTime

    default getCurrentSlot :: (MonadTrans t, MonadSlots m', t m' ~ m) => m SlotId
    getCurrentSlot = lift getCurrentSlot

instance MonadSlots m => MonadSlots (ReaderT s m) where
instance MonadSlots m => MonadSlots (ExceptT s m) where
instance MonadSlots m => MonadSlots (StateT s m) where
instance MonadSlots m => MonadSlots (DHTResponseT s m) where
instance MonadSlots m => MonadSlots (KademliaDHT m) where

-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m FlatSlotId
getCurrentSlotFlat = flattenSlotId <$> getCurrentSlot

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlots m => SlotId -> m Timestamp
getSlotStart (flattenSlotId -> slotId) =
    (Timestamp (fromIntegral slotId * slotDuration) +) <$> getSystemStartTime

getCurrentSlotUsingNtp :: (MonadSlots m, MonadTimed m)
                       => SlotId -> (Microsecond, Microsecond) -> m SlotId
getCurrentSlotUsingNtp lastSlot (margin, measTime) = do
    t <- (+ margin) <$> currentTime
    canTrust <- canWeTrustLocalTime t
    if canTrust then
        max lastSlot . f  <$>
            ((t -) . getTimestamp <$> getSystemStartTime)
    else pure lastSlot
  where
    f :: Microsecond -> SlotId
    f t = unflattenSlotId (fromIntegral $ t `div` slotDuration)
    -- We can trust getCurrentTime if it isn't bigger than:
    -- time for which we got margin (in last time) + NTP delay (+ some eps, for safety)
    canWeTrustLocalTime t =
        pure $ t <= measTime + ntpPollDelay + ntpMaxError

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses MonadTimed and assumes consistency between
-- MonadSlots and MonadTimed implementations.
onNewSlot
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithLogger m)
    => Bool -> (SlotId -> m ()) -> m a
onNewSlot = onNewSlotImpl False

-- | Same as onNewSlot, but also logs debug information.
onNewSlotWithLogging
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithLogger m)
    => Bool -> (SlotId -> m ()) -> m a
onNewSlotWithLogging = onNewSlotImpl True

onNewSlotImpl
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithLogger m)
    => Bool -> Bool -> (SlotId -> m ()) -> m a
onNewSlotImpl withLogging startImmediately action =
    onNewSlotDo withLogging Nothing startImmediately actionWithCatch
  where
    -- [CSL-198]: think about exceptions more carefully.
    actionWithCatch s = action s `catch` handler
    handler :: WithLogger m => SomeException -> m ()
    handler = logError . sformat ("Error occurred: "%build)

onNewSlotDo
    :: (MonadIO m, MonadTimed m, MonadSlots m, MonadCatch m, WithLogger m)
    => Bool -> Maybe SlotId -> Bool -> (SlotId -> m ()) -> m a
onNewSlotDo withLogging expectedSlotId startImmediately action = do
    -- here we wait for short intervals to be sure that expected slot
    -- has really started, taking into account possible inaccuracies
    waitUntilPredicate
        (maybe (const True) (<=) expectedSlotId <$> getCurrentSlot)
    curSlot <- getCurrentSlot
    -- fork is necessary because action can take more time than slotDuration
    when startImmediately $ fork_ $ action curSlot
    Timestamp curTime <- getCurrentTime
    let nextSlot = succ curSlot
    Timestamp nextSlotStart <- getSlotStart nextSlot
    let timeToWait = nextSlotStart - curTime
    when (timeToWait > 0) $
        do when withLogging $ logTTW timeToWait
           wait $ for timeToWait
    onNewSlotDo withLogging (Just nextSlot) True action
  where
    waitUntilPredicate predicate =
        unlessM predicate (shortWait >> waitUntilPredicate predicate)
    shortWaitTime = (10 :: Microsecond) `max` (slotDuration `div` 10000)
    shortWait = wait $ for shortWaitTime
    logTTW timeToWait = modifyLoggerName (<> "slotting") $ logDebug $
                 sformat ("Waiting for "%shown%" before new slot") timeToWait
