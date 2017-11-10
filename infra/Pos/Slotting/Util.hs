-- | Slotting utilities.

module Pos.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getCurrentSlotFlat
       , getSlotStart
       , getSlotStartPure
       , getSlotStartEmpatically
       , getCurrentEpochSlotDuration
       , getNextEpochSlotDuration
       , slotFromTimestamp

         -- * Worker which ticks when slot starts
       , onNewSlot

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart
       ) where

import           Universum

import           Data.Time.Units (Millisecond)
import           Formatting (int, sformat, shown, (%))
import           Mockable (Delay, Mockable, delay)
import           Serokell.Util (sec)
import           System.Wlog (WithLogger, logDebug, logInfo, logNotice, modifyLoggerName)

import           Pos.Core (FlatSlotId, HasConfiguration, LocalSlotIndex, SlotId (..),
                           Timestamp (..), flattenSlotId, slotIdF)
import           Pos.Recovery.Info (MonadRecoveryInfo, recoveryInProgress)
import           Pos.Reporting.Methods (MonadReporting, reportOrLogE)
import           Pos.Shutdown (HasShutdownContext)
import           Pos.Slotting.Class (MonadSlots (..))
import           Pos.Slotting.Error (SlottingError (..))
import           Pos.Slotting.Impl.Util (slotFromTimestamp)
import           Pos.Slotting.MemState (MonadSlotsData, getCurrentNextEpochSlottingDataM,
                                        getEpochSlottingDataM, getSystemStartM)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, computeSlotStart,
                                     lookupEpochSlottingData)
import           Pos.Util.Util (maybeThrow)


-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: (MonadSlots ctx m, HasConfiguration) => m (Maybe FlatSlotId)
getCurrentSlotFlat = fmap flattenSlotId <$> getCurrentSlot

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlotsData ctx m => SlotId -> m (Maybe Timestamp)
getSlotStart (SlotId {..}) = do
    systemStart        <- getSystemStartM
    mEpochSlottingData <- getEpochSlottingDataM siEpoch
    -- Maybe epoch slotting data.
    pure $ do
      epochSlottingData <- mEpochSlottingData
      pure $ computeSlotStart systemStart siSlot epochSlottingData

-- | Pure timestamp calculation for a given slot.
getSlotStartPure :: Timestamp -> SlotId -> SlottingData -> Maybe Timestamp
getSlotStartPure systemStart slotId slottingData =
    computeSlotStart systemStart localSlotIndex <$> epochSlottingData
  where
    epochSlottingData :: Maybe EpochSlottingData
    epochSlottingData = lookupEpochSlottingData (siEpoch slotId) slottingData

    localSlotIndex :: LocalSlotIndex
    localSlotIndex = siSlot slotId

-- | Get timestamp when given slot starts empatically, which means
-- that function throws exception when slot start is unknown.
getSlotStartEmpatically
    :: (MonadSlotsData ctx m, MonadThrow m)
    => SlotId
    -> m Timestamp
getSlotStartEmpatically slot =
    getSlotStart slot >>= maybeThrow (SEUnknownSlotStart slot)

-- | Get current slot duration.
getCurrentEpochSlotDuration
    :: (MonadSlotsData ctx m)
    => m Millisecond
getCurrentEpochSlotDuration =
    esdSlotDuration . fst <$> getCurrentNextEpochSlottingDataM

-- | Get last known slot duration.
getNextEpochSlotDuration
    :: (MonadSlotsData ctx m)
    => m Millisecond
getNextEpochSlotDuration =
    esdSlotDuration . snd <$> getCurrentNextEpochSlottingDataM

-- | Type constraint for `onNewSlot*` workers
type OnNewSlot ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , MonadSlots ctx m
    , MonadMask m
    , WithLogger m
    , Mockable Delay m
    , MonadReporting ctx m
    , HasShutdownContext ctx
    , MonadRecoveryInfo m
    , HasConfiguration
    )

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses Mockable and assumes consistency between
-- MonadSlots and Mockable implementations.
onNewSlot
    :: OnNewSlot ctx m
    => Bool -> (SlotId -> m ()) -> m ()
onNewSlot = onNewSlotImpl False

onNewSlotWithLogging
    :: OnNewSlot ctx m
    => Bool -> (SlotId -> m ()) -> m ()
onNewSlotWithLogging = onNewSlotImpl True

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall ctx m. OnNewSlot ctx m
    => Bool -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotImpl withLogging startImmediately action =
    impl `catch` workerHandler
  where
    impl = onNewSlotDo withLogging Nothing startImmediately actionWithCatch
    -- [CSL-1578] TODO: consider removing it.
    actionWithCatch s = action s `catch` actionHandler
    actionHandler :: SomeException -> m ()
    -- REPORT:ERROR 'reportOrLogE' in exception passed to 'onNewSlotImpl'.
    actionHandler = reportOrLogE "onNewSlotImpl: "
    workerHandler :: SomeException -> m ()
    workerHandler e = do
        -- REPORT:ERROR 'reportOrLogE' in 'onNewSlotImpl'
        reportOrLogE "Error occurred in 'onNewSlot' worker itself: " e
        delay =<< getNextEpochSlotDuration
        onNewSlotImpl withLogging startImmediately action

onNewSlotDo
    :: OnNewSlot ctx m
    => Bool -> Maybe SlotId -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotDo withLogging expectedSlotId startImmediately action = do
    curSlot <- waitUntilExpectedSlot

    -- Note that the action can take more time than the slot
    -- duration. In this case action for the next slot won't start
    -- until the ongoing action finishes.
    -- The caller should decide how to deal with long-running actions.
    -- They can do whatever they want with 'action', the simplest thing is
    -- 'void . fork' (which is likely not the best idea, for the reasons why
    -- 'async' package should be used instead of 'fork'-ing threads manually.
    --
    -- There are few things to consider when one wants to run this action in a
    -- separate thread every time:
    -- 1. If more than one action is launched, they may use same resources
    -- concurrently, so the code must account for it.
    -- 2. If action hangs for some reason, there can be infinitely growing pool
    -- of hanging actions with probably bad consequences.
    --
    -- See also: CSL-1606.
    when startImmediately $ action curSlot

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
        -- onNewSlotWorker doesn't make sense in recovery phase. Most
        -- definitely we don't know current slot and even if we do
        -- (same epoch), the only priority is to sync with the
        -- chain. So we're skipping and checking again.
        let skipRound = delay recoveryRefreshDelay >> waitUntilExpectedSlot
        ifM recoveryInProgress skipRound $ do
            slot <- getCurrentSlotBlocking
            if | maybe (const True) (<=) expectedSlotId slot -> return slot
            -- Here we wait for short intervals to be sure that expected slot
            -- has really started, taking into account possible inaccuracies.
            -- Usually it shouldn't happen.
               | otherwise -> delay shortDelay >> waitUntilExpectedSlot
    shortDelay :: Millisecond
    shortDelay = 42
    recoveryRefreshDelay :: Millisecond
    recoveryRefreshDelay = 150
    logTTW timeToWait = modifyLoggerName (<> "slotting") $ logDebug $
                 sformat ("Waiting for "%shown%" before new slot") timeToWait

logNewSlotWorker :: OnNewSlot ctx m => m ()
logNewSlotWorker =
    onNewSlotWithLogging True $ \slotId -> do
        modifyLoggerName (<> "slotting") $
            logNotice $ sformat ("New slot has just started: " %slotIdF) slotId

-- | Wait until system starts. This function is useful if node is
-- launched before 0-th epoch starts.
waitSystemStart
    :: ( MonadSlotsData ctx m
       , Mockable Delay m
       , WithLogger m
       , MonadSlots ctx m)
    => m ()
waitSystemStart = do
    start <- getSystemStartM
    cur <- currentTimeSlotting
    let Timestamp waitPeriod = start - cur
    when (cur < start) $ do
        logInfo $ sformat ("Waiting "%int%" seconds for system start") $
            waitPeriod `div` sec 1
        delay waitPeriod
