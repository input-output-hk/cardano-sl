{-# LANGUAGE DataKinds    #-}
{-# LANGUAGE TypeFamilies #-}

-- | NTP-based implementation of slotting.

module Pos.Slotting.Impl.Ntp
       (
         -- * State
         NtpSlottingState
       , NtpSlottingVar

         -- * Mode
       , NtpMode
       , NtpWorkerMode

         -- * MonadSlots, redirects, etc.
       , mkNtpSlottingVar

         -- * Methods
       , ntpGetCurrentSlot
       , ntpGetCurrentSlotBlocking
       , ntpGetCurrentSlotInaccurate
       , ntpCurrentTime

       -- * Workers
       , ntpWorkers
       ) where

import           Universum

import qualified Control.Concurrent.STM as STM
import           Control.Lens (makeLenses)
import qualified Data.List.NonEmpty as NE
import           Data.Time.Units (Microsecond)
import           Formatting (int, sformat, shown, stext, (%))
import           Mockable (CurrentTime, Delay, Mockable, Mockables, currentTime, delay)
import           NTP.Client (NtpClientSettings (..), NtpMonad, ntpSingleShot, spawnNtpClient)
import           Serokell.Util (sec)
import           System.Wlog (WithLogger, logDebug, logInfo, logWarning)

import           Pos.Core.Configuration (HasConfiguration)
import           Pos.Core.Slotting (EpochIndex, SlotId (..), Timestamp (..), unflattenSlotId)
import           Pos.Infra.Configuration (HasInfraConfiguration)
import qualified Pos.Infra.Configuration as Infra
import qualified Pos.Slotting.Configuration as C
import           Pos.Slotting.Impl.Util (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState (MonadSlotsData, getCurrentNextEpochIndexM,
                                        getCurrentNextEpochSlottingDataM, waitCurrentEpochEqualsM)
import           Pos.Util.Util (median)

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

-- | Data needed for the slotting algorithm to work.
data NtpSlottingState = NtpSlottingState
    {
    -- | Slot which was returned from getCurrentSlot last time.
      _nssLastSlot      :: !SlotId
    -- | Margin (difference between global time and local time) which
    -- we got from NTP server last time.
    , _nssLastMargin    :: !Microsecond
    -- | Time (local) for which we got margin in last time.
    , _nssLastLocalTime :: !Timestamp
    }

type NtpSlottingVar = TVar NtpSlottingState

makeLenses ''NtpSlottingState

mkNtpSlottingVar
    :: ( NtpMonad m
       , Mockables m
           [ CurrentTime
           , Delay
           ]
       , HasConfiguration
       , HasInfraConfiguration
       )
    => m NtpSlottingVar
mkNtpSlottingVar = do
    let _nssLastMargin = 0
    _nssLastLocalTime <- Timestamp <$> currentTime
    -- current time isn't quite valid value, but it doesn't matter (@pva701)
    let _nssLastSlot = unflattenSlotId 0
    res <- newTVarIO NtpSlottingState {..}
    -- We don't want to wait too much at the very beginning,
    -- 1 second should be enough.
    let settings = (ntpSettings res) { ntpResponseTimeout = 1 & sec }
    res <$ singleShot settings
  where
    singleShot settings = do
        logInfo $ "Waiting for response from NTP servers"
        ntpSingleShot settings

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type NtpMode ctx m =
    ( MonadIO m
    , MonadThrow m
    , WithLogger m
    , MonadSlotsData ctx m
    , Mockables m
        [ CurrentTime
        , Delay
        ]
    , HasConfiguration
    , HasInfraConfiguration
    )

type NtpWorkerMode m = (HasInfraConfiguration, NtpMonad m, Mockable Delay m)

----------------------------------------------------------------------------
-- MonadSlots implementation
----------------------------------------------------------------------------

ntpCurrentTime
    :: (NtpMode ctx m)
    => NtpSlottingVar -> m Timestamp
ntpCurrentTime var = do
    lastMargin <- view nssLastMargin <$> atomically (STM.readTVar var)
    Timestamp . (+ lastMargin) <$> currentTime

----------------------------------------------------------------------------
-- Getting current slot
----------------------------------------------------------------------------

data SlotStatus
    = CantTrust Text                    -- ^ We can't trust local time.
    | OutdatedSlottingData !EpochIndex  -- ^ We don't know recent
                                        -- slotting data, last known
                                        -- current epoch is attached.
    | CurrentSlot !SlotId               -- ^ Slot is calculated successfully.

ntpGetCurrentSlot
    :: (NtpMode ctx m)
    => NtpSlottingVar
    -> m (Maybe SlotId)
ntpGetCurrentSlot var = ntpGetCurrentSlotImpl var >>= \case
    CurrentSlot slot -> pure $ Just slot
    OutdatedSlottingData currentEpochIndex -> do
        logWarning $ sformat
            ("Can't get current slot, because slotting data"%
             " is outdated. Last known current epoch = "%int)
            currentEpochIndex
        Nothing <$ printSlottingData
    CantTrust t -> do
        logWarning $
            "Can't get current slot, because we can't trust local time, details: " <> t
        Nothing <$ printSlottingData
  where
    -- Here we could print all the slotting data
    printSlottingData = do
        (sd, _)  <- getCurrentNextEpochSlottingDataM
        logWarning $ "Slotting data: " <> show sd

ntpGetCurrentSlotInaccurate
    :: (NtpMode ctx m)
    => NtpSlottingVar -> m SlotId
ntpGetCurrentSlotInaccurate var = do
    res <- ntpGetCurrentSlotImpl var
    case res of
        CurrentSlot slot       -> pure slot
        CantTrust _            -> _nssLastSlot <$> atomically (STM.readTVar var)
        OutdatedSlottingData _ -> ntpCurrentTime var >>= approxSlotUsingOutdated

ntpGetCurrentSlotImpl
    :: (NtpMode ctx m)
    => NtpSlottingVar
    -> m SlotStatus
ntpGetCurrentSlotImpl var = do
    NtpSlottingState {..} <- atomically $ STM.readTVar var
    t <- Timestamp . (+ _nssLastMargin) <$> currentTime
    case canWeTrustLocalTime _nssLastLocalTime t of
      Nothing -> do
          (currentEpochIndex, _) <- getCurrentNextEpochIndexM
          res <- max _nssLastSlot <<$>> slotFromTimestamp t
          let setLastSlot s = atomically $ STM.modifyTVar' var (nssLastSlot %~ max s)
          whenJust res setLastSlot
          pure $ maybe (OutdatedSlottingData currentEpochIndex) CurrentSlot res
      Just reason -> pure $ CantTrust reason
  where
    -- We can trust getCurrentTime if it is:
    -- • not bigger than 'time for which we got margin (last time)
    --   + NTP delay (+ some eps, for safety)'
    -- • not less than 'last time - some eps'
    canWeTrustLocalTime :: Timestamp -> Timestamp -> Maybe Text
    canWeTrustLocalTime t1@(Timestamp lastLocalTime) t2@(Timestamp t) = do
        let ret = sformat ("T1: "%shown%", T2: "%shown%", reason: "%stext) t1 t2
        if | t > lastLocalTime + C.ntpPollDelay + C.ntpMaxError ->
             Just $ ret $ "curtime is bigger then last local: " <>
                    show C.ntpPollDelay <> ", " <> show C.ntpMaxError
           | t < lastLocalTime - C.ntpMaxError ->
             Just $ ret $ "curtime is less then last - error: " <> show C.ntpMaxError
           | otherwise -> Nothing

ntpGetCurrentSlotBlocking
    :: (NtpMode ctx m)
    => NtpSlottingVar -> m SlotId
ntpGetCurrentSlotBlocking var = ntpGetCurrentSlotImpl var >>= \case
    CantTrust _ -> do
        delay C.ntpPollDelay
        ntpGetCurrentSlotBlocking var
    OutdatedSlottingData current -> do
        waitCurrentEpochEqualsM (current + 1)
        ntpGetCurrentSlotBlocking var
    CurrentSlot slot -> pure slot

----------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------

-- | Workers necessary for NTP slotting.
ntpWorkers :: NtpWorkerMode m => NtpSlottingVar -> [m ()]
ntpWorkers = one . ntpSyncWorker

-- Worker for synchronization of local time and global time.
ntpSyncWorker
    :: NtpWorkerMode m
    => NtpSlottingVar -> m ()
ntpSyncWorker var = spawnNtpClient (ntpSettings var)

ntpHandlerDo
    :: (MonadIO m, WithLogger m)
    => NtpSlottingVar -> (Microsecond, Microsecond) -> m ()
ntpHandlerDo var (newMargin, transmitTime) = do
    logDebug $ sformat ("Callback on new margin: "%int% " mcs") newMargin
    let realTime = Timestamp $ transmitTime + newMargin
    atomically $ STM.modifyTVar var ( set nssLastMargin newMargin
                                    . set nssLastLocalTime realTime)

ntpSettings
    :: (HasInfraConfiguration, MonadIO m, WithLogger m)
    => NtpSlottingVar -> NtpClientSettings m
ntpSettings var = NtpClientSettings
    { -- list of servers addresses
      ntpServers         = Infra.ntpServers
    -- got time margin callback
    , ntpHandler         = ntpHandlerDo var
    -- logger name modifier
    , ntpLogName         = "ntp"
    -- delay between making requests and response collection;
    -- it also means that handler will be invoked with this lag
    , ntpResponseTimeout = C.ntpResponseTimeout
    -- how often to send responses to server
    , ntpPollDelay       = C.ntpPollDelay
    -- way to sumarize results received from different servers.
    , ntpMeanSelection   = median . NE.fromList
    }
