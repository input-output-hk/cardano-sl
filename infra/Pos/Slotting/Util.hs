{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting utilities.

module Pos.Slotting.Util
       (
         -- * Helpers using 'MonadSlots[Data]'
         getCurrentSlotFlat
       , getSlotStart
       , getSlotStartEmpatically
       , getLastKnownSlotDuration

         -- * Worker which ticks when slot starts
       , onNewSlot
       , onNewSlotImpl

         -- * Worker which logs beginning of new slot
       , logNewSlotWorker

         -- * Waiting for system start
       , waitSystemStart
       ) where

import           Data.Time.Units                  (Millisecond, convertUnit)
import           Formatting                       (build, int, sformat, shown, (%))
import           Mockable                         (Delay, Fork, Mockable, delay, fork)
import           Paths_cardano_sl_infra           (version)
import           Serokell.Util                    (sec)
import           System.Wlog                      (WithLogger, logDebug, logError,
                                                   logInfo, logNotice, modifyLoggerName)
import           Universum

import           Pos.Communication.Types.Protocol (NodeId)
import           Pos.Core.Slotting                (flattenSlotId)
import           Pos.Core.Types                   (FlatSlotId, SlotId (..),
                                                   Timestamp (..), slotIdF)
import           Pos.Exception                    (CardanoException)
import           Pos.Reporting.MemState           (MonadReportingMem)
import           Pos.Reporting.Methods            (reportMisbehaviourMasked,
                                                   reportingFatal)
import           Pos.Shutdown                     (MonadShutdownMem, runIfNotShutdown)
import           Pos.Slotting.Class               (MonadSlots (..))
import           Pos.Slotting.Error               (SlottingError (..))
import           Pos.Slotting.MemState            (MonadSlotsData (..))
import           Pos.Slotting.Types               (EpochSlottingData (..),
                                                   SlottingData (..))

-- TODO eliminate this copy-paste when would refactor Pos.Util
maybeThrow :: (MonadThrow m, Exception e) => e -> Maybe a -> m a
maybeThrow e = maybe (throwM e) pure

-- | Get flat id of current slot based on MonadSlots.
getCurrentSlotFlat :: MonadSlots m => m (Maybe FlatSlotId)
getCurrentSlotFlat = fmap flattenSlotId <$> getCurrentSlot

-- | Get timestamp when given slot starts.
getSlotStart :: MonadSlotsData m => SlotId -> m (Maybe Timestamp)
getSlotStart SlotId{..} = do
    SlottingData{..} <- getSlottingData
    if | siEpoch < sdPenultEpoch -> pure Nothing
       | siEpoch == sdPenultEpoch -> pure . Just $ slotTimestamp siSlot sdPenult
       | siEpoch == sdPenultEpoch + 1 -> pure . Just $ slotTimestamp siSlot sdLast
       | otherwise -> pure Nothing
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

-- | Get last known slot duration.
getLastKnownSlotDuration :: MonadSlotsData m => m Millisecond
getLastKnownSlotDuration = esdSlotDuration . sdLast <$> getSlottingData

-- | Type constraint for `onNewSlot*` workers
type OnNewSlot m =
    ( MonadIO m
    , MonadSlots m
    , MonadMask m
    , WithLogger m
    , Mockable Fork m
    , Mockable Delay m
    , MonadReportingMem m
    , MonadShutdownMem m
    )

-- | Run given action as soon as new slot starts, passing SlotId to
-- it.  This function uses Mockable and assumes consistency between
-- MonadSlots and Mockable implementations.
onNewSlot
    :: OnNewSlot m
    => m (Set NodeId) -> Bool -> (SlotId -> m ()) -> m ()
onNewSlot getPeers = onNewSlotImpl getPeers False

onNewSlotWithLogging
    :: OnNewSlot m
    => m (Set NodeId) -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotWithLogging getPeers = onNewSlotImpl getPeers True

-- TODO [CSL-198]: think about exceptions more carefully.
onNewSlotImpl
    :: forall m. OnNewSlot m
    => m (Set NodeId) -> Bool -> Bool -> (SlotId -> m ()) -> m ()
onNewSlotImpl getPeers withLogging startImmediately action =
    reportingFatal getPeers version impl `catch` workerHandler
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
        reportMisbehaviourMasked getPeers version msg
        delay =<< getLastKnownSlotDuration
        onNewSlotImpl getPeers withLogging startImmediately action

onNewSlotDo
    :: OnNewSlot m
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
    :: OnNewSlot m
    => m (Set NodeId) -> m ()
logNewSlotWorker getPeers =
    onNewSlotWithLogging getPeers True $ \slotId -> do
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
    start <- getSystemStart
    cur <- currentTimeSlotting
    let Timestamp waitPeriod = start - cur
    when (cur < start) $ do
        logInfo $ sformat ("Waiting "%int%" seconds for system start") $
            waitPeriod `div` sec 1
        delay waitPeriod
