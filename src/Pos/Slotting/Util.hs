{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting utilities.

module Pos.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getCurrentSlotFlat
       , getSlotStart
       , getSlotStartEmpatically

         -- * Worker which ticks when slot starts
       , onNewSlot
       , onNewSlotImpl

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart
       ) where

import           Control.Monad.Catch      (MonadCatch, catch)
import           Data.Time.Units          (Millisecond)
import           Data.Time.Units          (convertUnit)
import           Formatting               (build, int, sformat, shown, (%))
import           Mockable                 (Delay, Fork, Mockable, delay, fork)
import           Serokell.Util.Exceptions ()
import           System.Wlog              (WithLogger, logDebug, logError, logInfo,
                                           logNotice, modifyLoggerName)
import           Universum

import           Pos.Constants            (ntpMaxError, ntpPollDelay)
import           Pos.Context              (WithNodeContext (getNodeContext),
                                           ncSystemStart)
import           Pos.Exception            (CardanoException)
import           Pos.Slotting.Class       (MonadSlots (..), MonadSlotsData (..))
import           Pos.Slotting.Error       (SlottingError (..))
import           Pos.Slotting.Types       (EpochSlottingData (..), SlottingData (..))
import           Pos.Types                (FlatSlotId, SlotId (..), Timestamp (..),
                                           flattenSlotId, slotIdF)
import           Pos.Util                 (maybeThrow)
import           Pos.Util.Shutdown        (ifNotShutdown)
import           Pos.Util.TimeWarp        (minute, sec)

-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m (Maybe FlatSlotId)
getCurrentSlotFlat = fmap flattenSlotId <$> getCurrentSlot

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlotsData m => SlotId -> m (Maybe Timestamp)
getSlotStart SlotId{..} = do
    SlottingData{..} <- getSlottingData
    if | siEpoch < sdPenultEpoch -> pure Nothing
       | siEpoch == sdPenultEpoch -> pure . Just $ slotTimestamp siSlot sdPenult
       | otherwise -> pure . Just $ slotTimestamp siSlot sdLast
  where
    slotTimestamp locSlot EpochSlottingData{..} =
        esdStart + Timestamp (fromIntegral locSlot * convertUnit esdSlotDuration)

-- | Get timestamp when given slot starts empatically, which means
-- that function throws exception when slot start is unknown.
getSlotStartEmpatically
    :: (MonadSlotsData m, MonadThrow m)
    => SlotId -> m Timestamp
getSlotStartEmpatically slot =
    getSlotStart slot >>= maybeThrow (SEUnknownSlotStart slot)

-- | Type constraint for `onNewSlot*` workers
type OnNewSlot ssc m =
    ( MonadIO m
    , MonadSlots m
    , MonadCatch m
    , WithLogger m
    , Mockable Fork m
    , Mockable Delay m
    , WithNodeContext ssc m
    )

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses Mockable and assumes consistency between
-- MonadSlots and Mockable implementations.
onNewSlot
    :: OnNewSlot ssc m
    => Bool -> (SlotId -> m ()) -> m ()
onNewSlot = onNewSlotImpl False

onNewSlotWithLogging
    :: OnNewSlot ssc m
    => Bool -> (SlotId -> m ()) -> m ()
onNewSlotWithLogging = onNewSlotImpl True

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall ssc m.
       OnNewSlot ssc m
    => Bool -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotImpl withLogging startImmediately action =
    onNewSlotDo withLogging Nothing startImmediately actionWithCatch `catch`
    workerHandler
  where
    actionWithCatch s = action s `catch` actionHandler
    actionHandler
        :: forall m.
           WithLogger m
        => SomeException -> m ()
    actionHandler = logError . sformat ("Error occurred: " %build)
    workerHandler :: CardanoException -> m ()
    workerHandler e = do
        logError $
            sformat ("Error occurred in 'onNewSlot' worker itself: " %build) e
        delay $ minute 1

onNewSlotDo
    :: OnNewSlot ssc m
    => Bool -> Maybe SlotId -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotDo withLogging expectedSlotId startImmediately action = ifNotShutdown $ do
    curSlot <- waitUntilExpectedSlot

    -- Fork is necessary because action can take more time than duration of slot.
    when startImmediately $ void $ fork $ action curSlot

    -- check for shutdown flag again to not wait a whole slot
    ifNotShutdown $ do
        let nextSlot = succ curSlot
        Timestamp curTime <- currentTimeSlotting
        Timestamp nextSlotStart <- getSlotStartEmpatically nextSlot
        let timeToWait = nextSlotStart - curTime
        when (timeToWait > 0) $ do
            when withLogging $ logTTW timeToWait
            delay timeToWait
        onNewSlotDo withLogging (Just nextSlot) True action
  where
    waitUntilExpectedSlot = do
        slot <- getCurrentSlotBlocking
        if | maybe (const True) (<=) expectedSlotId slot -> return slot
        -- Here we wait for short intervals to be sure that expected slot
        -- has really started, taking into account possible inaccuracies.
        -- Usually it shouldn't happen.
           | otherwise -> delay shortDelay >> waitUntilExpectedSlot
    shortDelay :: Millisecond
    shortDelay = 42
    logTTW timeToWait = modifyLoggerName (<> "slotting") $ logDebug $
                 sformat ("Waiting for "%shown%" before new slot") timeToWait

logNewSlotWorker
    :: OnNewSlot ssc m
    => m ()
logNewSlotWorker =
    onNewSlotWithLogging True $ \slotId -> do
        modifyLoggerName (<> "slotting") $
            logNotice $ sformat ("New slot has just started: " %slotIdF) slotId

-- getSlotDuration = pure genesisSlotDuration

-- | Wait until system starts. This function is useful if node is
-- launched before 0-th epoch starts.
waitSystemStart
    :: (WithNodeContext ssc m, Mockable Delay m, WithLogger m, MonadSlots m)
    => m ()
waitSystemStart = do
    start <- ncSystemStart <$> getNodeContext
    cur <- currentTimeSlotting
    let Timestamp waitPeriod = start - cur
    when (cur < start) $ do
        logInfo $ sformat ("Waiting "%int%" seconds for system start") $
            waitPeriod `div` sec 1
        delay waitPeriod
