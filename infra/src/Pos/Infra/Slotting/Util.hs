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

import           Pos.Core (LocalSlotIndex, SlotId (..), Timestamp (..), slotIdF)
import           Pos.Core.Conc (delay, timeout)
import           Pos.Core.Slotting (ActionTerminationPolicy (..),
                     EpochSlottingData (..), MonadSlotsData,
                     OnNewSlotParams (..), SlottingData, computeSlotStart,
                     defaultOnNewSlotParams, getCurrentNextEpochSlottingDataM,
                     getCurrentSlotFlat, getEpochSlottingDataM,
                     getSystemStartM, lookupEpochSlottingData)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo, recoveryInProgress)
import           Pos.Infra.Reporting (MonadReporting, reportOrLogE)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting.Class (MonadSlots (..))
import           Pos.Infra.Slotting.Error (SlottingError (..))
import           Pos.Infra.Slotting.Impl.Util (slotFromTimestamp)
import           Pos.Util.Trace.Named (TraceNamed, appendName, logDebug,
                     logInfo, logNotice, logWarning)
import           Pos.Util.Util (maybeThrow)



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
    , MonadReporting m
    , HasShutdownContext ctx
    , MonadRecoveryInfo ctx m
    )

-- | Run given action as soon as new slot starts, passing SlotId to it.
onNewSlot
    :: MonadOnNewSlot ctx m
    => TraceNamed m -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlot logTrace = onNewSlotImpl logTrace False

onNewSlotWithLogging
    :: MonadOnNewSlot ctx m
    => TraceNamed m -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotWithLogging logTrace = onNewSlotImpl logTrace True

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall ctx m. MonadOnNewSlot ctx m
    => TraceNamed m -> Bool -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotImpl logTrace withLogging params action =
    impl `catch` workerHandler
  where
    impl = onNewSlotDo logTrace withLogging Nothing params actionWithCatch
    -- [CSL-198] TODO: consider removing it.
    actionWithCatch s = action s `catch` actionHandler
    actionHandler :: SomeException -> m ()
    -- REPORT:ERROR 'reportOrLogE' in exception passed to 'onNewSlotImpl'.
    actionHandler = reportOrLogE logTrace "onNewSlotImpl: "
    workerHandler :: SomeException -> m ()
    workerHandler e = do
        -- REPORT:ERROR 'reportOrLogE' in 'onNewSlotImpl'
        reportOrLogE logTrace "Error occurred in 'onNewSlot' worker itself: " e
        delay =<< getNextEpochSlotDuration
        onNewSlotImpl logTrace withLogging params action

onNewSlotDo
    :: MonadOnNewSlot ctx m
    => TraceNamed m -> Bool -> Maybe SlotId -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotDo logTrace withLogging expectedSlotId onsp action = do
    curSlot <- waitUntilExpectedSlot

    let nextSlot = succ curSlot
    Timestamp curTime <- currentTimeSlotting
    Timestamp nextSlotStart <- getSlotStartEmpatically nextSlot
    let timeToWait = nextSlotStart - curTime

    let applyTimeout a = case onspTerminationPolicy onsp of
          NoTerminationPolicy -> a
          NewSlotTerminationPolicy name ->
              whenNothingM_ (timeout timeToWait a) $
                  logWarning logTrace $ sformat
                  ("Action "%stext%
                   " hasn't finished before new slot started") name

    when (onspStartImmediately onsp) $ applyTimeout $ action curSlot

    when (timeToWait > 0) $ do
        logTTW timeToWait
        delay timeToWait
    let newParams = onsp { onspStartImmediately = True }
    onNewSlotDo logTrace withLogging (Just nextSlot) newParams action
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
    logTTW timeToWait = logDebug (appendName "slotting" logTrace) $ sformat ("Waiting for "%shown%" before new slot") timeToWait

logNewSlotWorker
    :: MonadOnNewSlot ctx m
    => TraceNamed m
    ->  m ()
logNewSlotWorker logTrace =
    onNewSlotWithLogging logTrace defaultOnNewSlotParams $ \slotId -> do
        logNotice (appendName "slotting" logTrace) $ sformat ("New slot has just started: " %slotIdF) slotId

-- | Wait until system starts. This function is useful if node is
-- launched before 0-th epoch starts.
waitSystemStart
    :: ( MonadSlotsData ctx m
       , MonadSlots ctx m)
    => TraceNamed m
    -> m ()
waitSystemStart logTrace = do
    start <- getSystemStartM
    cur <- currentTimeSlotting
    let Timestamp waitPeriod = start - cur
    when (cur < start) $ do
        logInfo logTrace $ sformat ("Waiting "%int%" seconds for system start") $
            waitPeriod `div` fromMicroseconds 1000000
        delay waitPeriod
