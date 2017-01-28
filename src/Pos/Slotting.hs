{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Slotting functionality.

module Pos.Slotting
       ( MonadSlots (..)
       , getCurrentSlotFlat
       , getSlotStart
       , getCurrentSlotUsingNtp

       , onNewSlot
       , onNewSlotWithLogging

       -- * Slotting state
       , SlottingState (..)
       , ssSlotDurationL
       , ssNtpLastSlotL
       , ssNtpDataL
       ) where

import           Control.Lens             (makeLensesFor)
import           Control.Monad            (void)
import           Control.Monad.Catch      (MonadCatch, catch)
import           Control.Monad.Except     (ExceptT)
import           Control.Monad.Trans      (MonadTrans)
import           Data.Time.Units          (Microsecond, Millisecond, convertUnit)
import           Formatting               (build, sformat, shown, (%))
import           Mockable                 (CurrentTime, Delay, Fork, Mockable,
                                           currentTime, delay, fork)
import           Serokell.Util.Exceptions ()
import           System.Wlog              (WithLogger, logDebug, logError,
                                           modifyLoggerName)
import           Universum

import           Pos.Constants            (ntpMaxError, ntpPollDelay)
import           Pos.DHT.Real             (KademliaDHT)
import           Pos.Types                (FlatSlotId, SlotId (..), Timestamp (..),
                                           flattenSlotId, unflattenSlotId)

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

-- | Data needed for the slotting algorithm to work.
data SlottingState = SlottingState
    {
    -- | Current slot duration. (It can be changed by update proposals.)
      ssSlotDuration :: !Millisecond
    -- | Slot which was returned from getCurrentSlot in last time
    , ssNtpLastSlot  :: !SlotId
    -- | Data for the NTP Worker. First element: margin (difference between
    -- global time and local time) which we got from NTP server in last time.
    -- Second element: time (local) for which we got margin in last time.
    , ssNtpData      :: !(Microsecond, Microsecond)
    }

flip makeLensesFor ''SlottingState [
    ("ssSlotDuration", "ssSlotDurationL"),
    ("ssNtpLastSlot", "ssNtpLastSlotL"),
    ("ssNtpData", "ssNtpDataL") ]

-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m FlatSlotId
getCurrentSlotFlat = flattenSlotId <$> getCurrentSlot

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlots m => SlotId -> m Timestamp
getSlotStart (flattenSlotId -> slotId) = do
    slotDuration <- getSlotDuration
    startTime    <- getSystemStartTime
    return $ startTime +
             Timestamp (fromIntegral slotId * convertUnit slotDuration)

getCurrentSlotUsingNtp :: (MonadSlots m, Mockable CurrentTime m)
                       => SlotId -> (Microsecond, Microsecond) -> m SlotId
getCurrentSlotUsingNtp lastSlot (margin, measTime) = do
    t <- (+ margin) <$> currentTime
    canTrust <- canWeTrustLocalTime t
    slotDuration <- getSlotDuration
    if canTrust then
        max lastSlot . f (convertUnit slotDuration) <$>
            ((t -) . getTimestamp <$> getSystemStartTime)
    else pure lastSlot
  where
    f :: Microsecond -> Microsecond -> SlotId
    f slotDuration diff
        | diff < 0 = SlotId 0 0
        | otherwise = unflattenSlotId (fromIntegral $ diff `div` slotDuration)
    -- We can trust getCurrentTime if it isn't bigger than:
    -- time for which we got margin (in last time) + NTP delay (+ some eps, for safety)
    canWeTrustLocalTime t =
        pure $ t <= measTime + ntpPollDelay + ntpMaxError

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses Mockable and assumes consistency between
-- MonadSlots and Mockable implementations.
onNewSlot
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       )
    => Bool -> (SlotId -> m ()) -> m a
onNewSlot = onNewSlotImpl False

-- | Same as onNewSlot, but also logs debug information.
onNewSlotWithLogging
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       )
    => Bool -> (SlotId -> m ()) -> m a
onNewSlotWithLogging = onNewSlotImpl True

onNewSlotImpl
    :: ( MonadIO m
       , MonadSlots m
       , MonadCatch m
       , WithLogger m
       , Mockable Fork m
       , Mockable Delay m
       )
    => Bool -> Bool -> (SlotId -> m ()) -> m a
onNewSlotImpl withLogging startImmediately action =
    onNewSlotDo withLogging Nothing startImmediately actionWithCatch
  where
    -- TODO [CSL-198]: think about exceptions more carefully.
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
    => Bool -> Maybe SlotId -> Bool -> (SlotId -> m ()) -> m a
onNewSlotDo withLogging expectedSlotId startImmediately action = do
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
    when (timeToWait > 0) $ do
        when withLogging $ logTTW timeToWait
        delay timeToWait
    onNewSlotDo withLogging (Just nextSlot) True action
  where
    waitUntilPredicate predicate =
        unlessM predicate (shortWait >> waitUntilPredicate predicate)
    shortWait = do
        slotDuration <- getSlotDuration
        delay ((10 :: Microsecond) `max` (convertUnit slotDuration `div` 10000))
    logTTW timeToWait = modifyLoggerName (<> "slotting") $ logDebug $
                 sformat ("Waiting for "%shown%" before new slot") timeToWait
