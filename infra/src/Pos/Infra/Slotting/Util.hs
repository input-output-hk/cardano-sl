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
       , onNewSlotNoLogging

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart

         -- * Re-exported from the Sinbin
       , ActionTerminationPolicy (..)
       , OnNewSlotParams (..)
       , defaultOnNewSlotParams
       , getCurrentSlotFlat
       ) where

import           Universum

import           Data.Time.Units (Millisecond, fromMicroseconds)
import           Formatting (int, sformat, shown, stext, (%))
import           Mockable (Async, Delay, Mockable, delay, timeout)

import           Pos.Core (HasProtocolConstants, LocalSlotIndex, SlotId (..),
                     Timestamp (..), slotIdF)
import           Pos.Infra.Recovery.Info (MonadRecoveryInfo, recoveryInProgress)
import           Pos.Infra.Shutdown (HasShutdownContext)
import           Pos.Infra.Slotting.Class (MonadSlots (..))
import           Pos.Infra.Slotting.Error (SlottingError (..))
import           Pos.Infra.Slotting.Impl.Util (slotFromTimestamp)
import           Pos.Infra.Slotting.MemState (getCurrentNextEpochSlottingDataM,
                     getEpochSlottingDataM, getSystemStartM)
import           Pos.Sinbin.Reporting (MonadReporting, reportOrLogE)
import           Pos.Sinbin.Slotting (EpochSlottingData (..), MonadSlotsData,
                     SlottingData, computeSlotStart, lookupEpochSlottingData)
import           Pos.Sinbin.Slotting.Util (ActionTerminationPolicy (..),
                     OnNewSlotParams (..), defaultOnNewSlotParams,
                     getCurrentSlotFlat)
import           Pos.Util.Trace (noTrace)
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
    , MonadReader ctx m
    , MonadSlots ctx m
    , MonadMask m
    , Mockable Async m
    , Mockable Delay m
    , MonadReporting m
    , HasShutdownContext ctx
    , MonadRecoveryInfo m
    )

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses Mockable and assumes consistency between
-- MonadSlots and Mockable implementations.
onNewSlot
    :: (MonadOnNewSlot ctx m, HasProtocolConstants)
    => TraceNamed m -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlot logTrace = onNewSlotImpl logTrace

-- TODO just temporary solution
onNewSlotNoLogging
    :: ( MonadOnNewSlot ctx m
       , HasProtocolConstants
       )
    => OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotNoLogging = onNewSlot noTrace

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall ctx m. (MonadOnNewSlot ctx m, HasProtocolConstants)
    => TraceNamed m -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotImpl logTrace params action =
    impl `catch` workerHandler
  where
    impl = onNewSlotDo logTrace Nothing params actionWithCatch
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
        onNewSlotImpl logTrace params action

onNewSlotDo
    :: (MonadOnNewSlot ctx m, HasProtocolConstants)
    => TraceNamed m -> Maybe SlotId -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotDo logTrace expectedSlotId onsp action = do
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
    onNewSlotDo logTrace (Just nextSlot) newParams action
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
    :: (MonadOnNewSlot ctx m, HasProtocolConstants)
    => TraceNamed m
    ->  m ()
logNewSlotWorker logTrace =
    onNewSlot logTrace defaultOnNewSlotParams $ \slotId -> do
        logNotice (appendName "slotting" logTrace) $ sformat ("New slot has just started: " %slotIdF) slotId

-- | Wait until system starts. This function is useful if node is
-- launched before 0-th epoch starts.
waitSystemStart
    :: ( MonadSlotsData ctx m
       , Mockable Delay m
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
