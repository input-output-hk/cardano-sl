{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting utilities.

module Pos.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getCurrentSlotFlat
       , getSlotStart
       , getSlotStartPure
       , getSlotStartEmpatically
       , getNextEpochSlotDuration

         -- * Worker which ticks when slot starts
       , onNewSlot
       , onNewSlotImpl

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart
       ) where

import           Data.Time.Units        (Millisecond)
import           Formatting             (build, int, sformat, shown, (%))
import           Mockable               (Delay, Fork, Mockable, delay, fork)
import           Serokell.Util          (sec)
import           System.Wlog            (WithLogger, logDebug, logError, logInfo,
                                         logNotice, modifyLoggerName)
import           Universum

import           Pos.Core               (FlatSlotId, SlotId (..), Timestamp (..),
                                         flattenSlotId, slotIdF)
import           Pos.Discovery.Class    (MonadDiscovery)
import           Pos.Exception          (CardanoException)
import           Pos.Recovery.Info      (MonadRecoveryInfo (recoveryInProgress))
import           Pos.Reporting.MemState (HasReportingContext)
import           Pos.Reporting.Methods  (reportMisbehaviourSilent, reportingFatal)
import           Pos.Shutdown           (HasShutdownContext, runIfNotShutdown)
import           Pos.Slotting.Class     (MonadSlots (..))
import           Pos.Slotting.Error     (SlottingError (..))
import           Pos.Slotting.MemState  (MonadSlotsData (..))
import           Pos.Slotting.Types     (EpochSlottingData (..), SlottingData,
                                         computeSlotStart, lookupEpochSlottingData)
import           Pos.Util.Util          (maybeThrow)


-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m (Maybe FlatSlotId)
getCurrentSlotFlat = fmap flattenSlotId <$> getCurrentSlot

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlotsData m => SlotId -> m (Maybe Timestamp)
getSlotStart si@(SlotId {..}) = do
    systemStart        <- getSystemStartM
    mEpochSlottingData <- getEpochSlottingDataM siEpoch
    -- Maybe epoch slotting data.
    pure $ do
      epochSlottingData <- mEpochSlottingData
      pure $ computeSlotStart systemStart si epochSlottingData

-- | Pure timestamp calculation for a given slot.
getSlotStartPure :: Timestamp -> SlotId -> SlottingData -> Maybe Timestamp
getSlotStartPure systemStart si sd =
    computeSlotStart systemStart si <$> esd
  where
    esd = lookupEpochSlottingData (siEpoch si) sd

-- | Get timestamp when given slot starts empatically, which means
-- that function throws exception when slot start is unknown.
getSlotStartEmpatically
    :: (MonadSlotsData m, MonadThrow m)
    => SlotId
    -> m Timestamp
getSlotStartEmpatically slot =
    getSlotStart slot >>= maybeThrow (SEUnknownSlotStart slot)

-- | Get last known slot duration.
getNextEpochSlotDuration
    :: (MonadSlotsData m)
    => m Millisecond
getNextEpochSlotDuration = esdSlotDuration <$> getNextEpochSlottingDataM

-- | Type constraint for `onNewSlot*` workers
type OnNewSlot ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , MonadSlots m
    , MonadMask m
    , WithLogger m
    , Mockable Fork m
    , Mockable Delay m
    , HasReportingContext ctx
    , HasShutdownContext ctx
    , MonadDiscovery m
    , MonadRecoveryInfo m
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
    reportingFatal impl `catch` workerHandler
  where
    impl = onNewSlotDo withLogging Nothing startImmediately actionWithCatch
    actionWithCatch s = action s `catch` actionHandler
    actionHandler
        :: forall ma.
           WithLogger ma
        => SomeException -> ma ()
    actionHandler = logError . sformat ("Error occurred: " %build)
    workerHandler :: CardanoException -> m ()
    workerHandler e = do
        let msg = sformat ("Error occurred in 'onNewSlot' worker itself: " %build) e
        logError $ msg
        -- [CSL-1340] FIXME: it's not misbehavior, it should be reported as 'RError'.
        reportMisbehaviourSilent False msg
        delay =<< getNextEpochSlotDuration
        onNewSlotImpl withLogging startImmediately action

onNewSlotDo
    :: OnNewSlot ctx m
    => Bool -> Maybe SlotId -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotDo withLogging expectedSlotId startImmediately action = runIfNotShutdown $ do
    curSlot <- waitUntilExpectedSlot

    -- Fork is necessary because action can take more time than duration of slot.
    when startImmediately $ void $ fork $ action curSlot

    -- check for shutdown flag again to not wait a whole slot
    runIfNotShutdown $ do
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
    :: ( MonadSlotsData m
       , Mockable Delay m
       , WithLogger m
       , MonadSlots m)
    => m ()
waitSystemStart = do
    start <- getSystemStartM
    cur <- currentTimeSlotting
    let Timestamp waitPeriod = start - cur
    when (cur < start) $ do
        logInfo $ sformat ("Waiting "%int%" seconds for system start") $
            waitPeriod `div` sec 1
        delay waitPeriod
