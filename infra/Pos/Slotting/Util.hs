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

         -- * Worker which ticks when slot starts and its parameters
       , OnNewSlotParams (..)
       , defaultOnNewSlotParams
       , ActionTerminationPolicy (..)
       , onNewSlot

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart
       ) where

import           Universum

import           Data.Time.Units (Millisecond)
import           Formatting (int, sformat, shown, stext, (%))
import           Mockable (Async, Delay, Mockable, delay, timeout)
import           Serokell.Util (sec)
import           System.Wlog (WithLogger, logDebug, logInfo, logNotice, logWarning,
                              modifyLoggerName)

import           Pos.Core (FlatSlotId, LocalSlotIndex, SlotId (..), HasProtocolConstants,
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
getCurrentSlotFlat :: (MonadSlots ctx m, HasProtocolConstants) => m (Maybe FlatSlotId)
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
type MonadOnNewSlot ctx m =
    ( MonadIO m
    , MonadReader ctx m
    , MonadSlots ctx m
    , MonadMask m
    , WithLogger m
    , Mockable Async m
    , Mockable Delay m
    , MonadReporting m
    , HasShutdownContext ctx
    , MonadRecoveryInfo m
    )

-- | Parameters for `onNewSlot`.
data OnNewSlotParams = OnNewSlotParams
    { onspStartImmediately  :: !Bool
    -- ^ Whether first action should be executed ASAP (i. e. basically
    -- when the program starts), or only when new slot starts.
    --
    -- For example, if the program is started in the middle of a slot
    -- and this parameter in 'False', we will wait for half of slot
    -- and only then will do something.
    , onspTerminationPolicy :: !ActionTerminationPolicy
    -- ^ What should be done if given action doesn't finish before new
    -- slot starts. See the description of 'ActionTerminationPolicy'.
    }

-- | Default parameters which were used by almost all code before this
-- data type was introduced.
defaultOnNewSlotParams :: OnNewSlotParams
defaultOnNewSlotParams =
    OnNewSlotParams
    { onspStartImmediately = True
    , onspTerminationPolicy = NoTerminationPolicy
    }

-- | This policy specifies what should be done if the action passed to
-- `onNewSlot` doesn't finish when current slot finishes.
--
-- We don't want to run given action more than once in parallel for
-- variety of reasons:
-- 1. If action hangs for some reason, there can be infinitely growing pool
-- of hanging actions with probably bad consequences (e. g. leaking memory).
-- 2. Thread management will be quite complicated if we want to fork
-- threads inside `onNewSlot`.
-- 3. If more than one action is launched, they may use same resources
-- concurrently, so the code must account for it.
data ActionTerminationPolicy
    = NoTerminationPolicy
    -- ^ Even if action keeps running after current slot finishes,
    -- we'll just wait and start action again only after the previous
    -- one finishes.
    | NewSlotTerminationPolicy !Text
    -- ^ If new slot starts, running action will be cancelled. Name of
    -- the action should be passed for logging.

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses Mockable and assumes consistency between
-- MonadSlots and Mockable implementations.
onNewSlot
    :: (MonadOnNewSlot ctx m, HasProtocolConstants)
    => OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlot = onNewSlotImpl False

onNewSlotWithLogging
    :: (MonadOnNewSlot ctx m, HasProtocolConstants)
    => OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotWithLogging = onNewSlotImpl True

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall ctx m. (MonadOnNewSlot ctx m, HasProtocolConstants)
    => Bool -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotImpl withLogging params action =
    impl `catch` workerHandler
  where
    impl = onNewSlotDo withLogging Nothing params actionWithCatch
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
        onNewSlotImpl withLogging params action

onNewSlotDo
    :: (MonadOnNewSlot ctx m, HasProtocolConstants)
    => Bool -> Maybe SlotId -> OnNewSlotParams -> (SlotId -> m ()) -> m ()
onNewSlotDo withLogging expectedSlotId onsp action = do
    curSlot <- waitUntilExpectedSlot

    let nextSlot = succ curSlot
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
    onNewSlotDo withLogging (Just nextSlot) newParams action
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

logNewSlotWorker :: (MonadOnNewSlot ctx m, HasProtocolConstants) => m ()
logNewSlotWorker =
    onNewSlotWithLogging defaultOnNewSlotParams $ \slotId -> do
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
