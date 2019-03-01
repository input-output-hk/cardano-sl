{-# LANGUAGE RecordWildCards #-}

-- | Slotting utilities.

module Pos.Infra.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getSlotStart
       , getSlotStartPure
       , getSlotStartEmpatically
       , getCurrentEpochSlotDuration
       , getNextEpochSlotDuration
       , slotFromTimestamp

         -- * Worker which ticks when slot starts and its parameters
       , onNewSlot

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart

         -- * Re-exported from Core
       , ActionTerminationPolicy (..)
       , OnNewSlotParams (..)
       , defaultOnNewSlotParams
       , getCurrentSlotFlat
       ) where

import           Universum

import           Data.Time.Units (Millisecond, fromMicroseconds)
import           Formatting (int, sformat, shown, stext, (%))
import           UnliftIO (MonadUnliftIO)

import           Pos.Core (LocalSlotIndex, SlotCount, SlotId (..),
                     Timestamp (..), slotIdF, slotIdSucc)
import           Pos.Core.Conc (delay, timeout)
import           Pos.Core.Slotting (ActionTerminationPolicy (..),
                     EpochSlottingData (..), MonadSlotsData,
                     OnNewSlotParams (..), SlottingData, TimeDiff (..),
                     computeSlotStart, defaultOnNewSlotParams,
                     getCurrentNextEpochSlottingDataM, getCurrentSlotFlat,
                     getEpochSlottingDataM, getSystemStartM,
                     lookupEpochSlottingData)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo, recoveryInProgress)
import           Pos.Infra.Reporting (MonadReporting, reportOrLogE)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting.Class (MonadSlots (..))
import           Pos.Infra.Slotting.Error (SlottingError (..))
import           Pos.Infra.Slotting.Impl.Util (slotFromTimestamp)
import           Pos.Util.Log.Structured (logInfoSX)
import           Pos.Util.Util (maybeThrow)
import           Pos.Util.Wlog (WithLogger, logDebug, logInfo, logNotice,
                     logWarning, modifyLoggerName)


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
type MonadOnNewSlot ctx m =
    ( MonadIO m
    , MonadUnliftIO m
    , MonadReader ctx m
    , MonadSlots ctx m
    , MonadMask m
    , WithLogger m
    , MonadReporting m
    , HasShutdownContext ctx
    , MonadRecoveryInfo ctx m
    )

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.
onNewSlot
    :: MonadOnNewSlot ctx m
    => SlotCount -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlot epochSlots = onNewSlotImpl epochSlots False

onNewSlotWithLogging
    :: MonadOnNewSlot ctx m
    => SlotCount -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotWithLogging epochSlots = onNewSlotImpl epochSlots True

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall ctx m
     . MonadOnNewSlot ctx m
    => SlotCount -> Bool -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotImpl epochSlots withLogging params action =
    impl `catch` workerHandler
  where
    impl = onNewSlotDo epochSlots withLogging Nothing params actionWithCatch
    -- [CSL-198] TODO: consider removing it.
    actionWithCatch s = action s `catch` actionHandler
    actionHandler :: SomeException -> m ()
    -- REPORT:ERROR 'reportOrLogE' in exception passed to 'onNewSlotImpl'.
    actionHandler = reportOrLogE "onNewSlotImpl: "
    workerHandler :: SomeException -> m ()
    workerHandler e = do
        -- REPORT:ERROR 'reportOrLogE' in 'onNewSlotImpl'
        reportOrLogE "Error occurred in 'onNewSlot' worker itself: " e
        delay =<< getNextEpochSlotDuration
        onNewSlotImpl epochSlots withLogging params action

onNewSlotDo
    :: MonadOnNewSlot ctx m
    => SlotCount -> Bool -> Maybe SlotId -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotDo epochSlots withLogging expectedSlotId onsp action = do
    curSlot <- waitUntilExpectedSlot

    let nextSlot = slotIdSucc epochSlots curSlot
    logDebug $ sformat ("onNewSlotDo: curSlot = "%shown) curSlot
    logDebug $ sformat ("onNewSlotDo: nextSlot = "%shown) nextSlot
    Timestamp curTime <- currentTimeSlotting
    Timestamp nextSlotStart <- getSlotStartEmpatically nextSlot
    let timeToWait = nextSlotStart - curTime

    let applyTimeout a = case onspTerminationPolicy onsp of
          NoTerminationPolicy -> a
          NewSlotTerminationPolicy name ->
              whenNothingM_ (timeout timeToWait a) $
                  logWarning $ sformat
                  ("Action "%stext%
                   " hasn't finished before new slot started") name

    when (onspStartImmediately onsp) $ applyTimeout $ action curSlot

    when (timeToWait > 0) $ do
        when withLogging $ logTTW timeToWait
        delay timeToWait
    let newParams = onsp { onspStartImmediately = True }
    onNewSlotDo epochSlots withLogging (Just nextSlot) newParams action
  where
    waitUntilExpectedSlot = do
        -- onNewSlotWorker doesn't make sense in recovery phase. Most
        -- definitely we don't know current slot and even if we do
        -- (same epoch), the only priority is to sync with the
        -- chain. So we're skipping and checking again.
        let skipRound = delay recoveryRefreshDelay >> waitUntilExpectedSlot
        ifM (recoveryInProgress epochSlots) skipRound $ do
            slot <- getCurrentSlotBlocking epochSlots
            if | maybe (const True) (<=) expectedSlotId slot -> return slot
            -- Here we wait for short intervals to be sure that expected slot
            -- has really started, taking into account possible inaccuracies.
            -- Usually it shouldn't happen.
               | otherwise -> delay shortDelay >> waitUntilExpectedSlot
    shortDelay :: Millisecond
    shortDelay = 42
    recoveryRefreshDelay :: Millisecond
    recoveryRefreshDelay = 150
    logTTW timeToWait = modifyLoggerName (<> ".slotting") $ do
        logDebug $ sformat ("Waiting for "%shown%" before new slot") timeToWait
        logInfoSX $ TimeDiff timeToWait

logNewSlotWorker :: MonadOnNewSlot ctx m => SlotCount -> m ()
logNewSlotWorker epochSlots =
    onNewSlotWithLogging epochSlots defaultOnNewSlotParams $ \slotId -> do
        modifyLoggerName (<> ".slotting") $
            logNotice $ sformat ("New slot has just started: " %slotIdF) slotId

-- | Wait until system starts. This function is useful if node is
-- launched before 0-th epoch starts.
waitSystemStart
    :: ( MonadSlotsData ctx m
       , WithLogger m
       , MonadSlots ctx m)
    => m ()
waitSystemStart = do
    start <- getSystemStartM
    cur <- currentTimeSlotting
    let Timestamp waitPeriod = start - cur
    when (cur < start) $ do
        logInfo $ sformat ("Waiting "%int%" seconds for system start") $
            waitPeriod `div` fromMicroseconds 1000000
        delay waitPeriod
