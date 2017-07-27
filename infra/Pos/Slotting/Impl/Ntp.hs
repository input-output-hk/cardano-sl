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

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (makeLenses)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.List                   ((!!))
import           Data.Time.Units             (Microsecond)
import           Formatting                  (int, sformat, shown, stext, (%))
import           Mockable                    (Catch, CurrentTime, Delay, Fork, Mockables,
                                              Throw, currentTime, delay)
import           NTP.Client                  (NtpClientSettings (..), ntpSingleShot,
                                              startNtpClient)
import           NTP.Example                 ()
import           Serokell.Util               (sec)
import           System.Wlog                 (WithLogger, logDebug, logInfo, logWarning)

import qualified Pos.Core.Constants          as C
import           Pos.Core.Slotting           (unflattenSlotId)
import           Pos.Core.Types              (EpochIndex, SlotId (..), Timestamp (..))
import qualified Pos.Slotting.Constants      as C
import           Pos.Slotting.Impl.Util      (approxSlotUsingOutdated, slotFromTimestamp)
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))

----------------------------------------------------------------------------
-- TODO
----------------------------------------------------------------------------

-- TODO: it's not exported from 'node-sketch' and it's too hard to do
-- it because of the mess in 'node-sketch' branches.
--
-- It should be exported and used here, I think.
type NtpMonad m =
    ( MonadIO m
    , MonadBaseControl IO m
    , WithLogger m
    , Mockables m
        [ Fork
        , Throw
        , Catch
        ]
    , MonadMask m
    )

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
    singleShot settings = unless C.isDevelopment $ do
        logInfo $ "Waiting for response from NTP servers"
        ntpSingleShot settings

----------------------------------------------------------------------------
-- Mode
----------------------------------------------------------------------------

type NtpMode m =
    ( MonadIO m
    , MonadThrow m
    , WithLogger m
    , MonadSlotsData m
    , Mockables m
        [ CurrentTime
        , Delay
        ]
    )

type NtpWorkerMode m = NtpMonad m

----------------------------------------------------------------------------
-- MonadSlots implementation
----------------------------------------------------------------------------

ntpCurrentTime
    :: (NtpMode m)
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
    :: (NtpMode m)
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
        sd  <- getCurrentEpochSlottingDataM
        logWarning $ "Slotting data: " <> show sd

ntpGetCurrentSlotInaccurate
    :: (NtpMode m)
    => NtpSlottingVar -> m SlotId
ntpGetCurrentSlotInaccurate var = do
    res <- ntpGetCurrentSlotImpl var
    case res of
        CurrentSlot slot -> pure slot
        CantTrust _        -> do
            _nssLastSlot <$> atomically (STM.readTVar var)
        OutdatedSlottingData currentEpochIndex ->
            ntpCurrentTime var >>= approxSlotUsingOutdated currentEpochIndex

ntpGetCurrentSlotImpl
    :: (NtpMode m)
    => NtpSlottingVar
    -> m SlotStatus
ntpGetCurrentSlotImpl var = do
    NtpSlottingState {..} <- atomically $ STM.readTVar var
    t <- Timestamp . (+ _nssLastMargin) <$> currentTime
    case canWeTrustLocalTime _nssLastLocalTime t of
      Nothing -> do
          currentEpochIndex <- getCurrentEpochIndexM
          res <- fmap (max _nssLastSlot) <$> slotFromTimestamp t
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
    :: (NtpMode m, MonadThrow m)
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
ntpSyncWorker = void . startNtpClient . ntpSettings

ntpHandlerDo
    :: (MonadIO m, WithLogger m)
    => NtpSlottingVar -> (Microsecond, Microsecond) -> m ()
ntpHandlerDo var (newMargin, transmitTime) = do
    logDebug $ sformat ("Callback on new margin: "%int% " mcs") newMargin
    let realTime = Timestamp $ transmitTime + newMargin
    atomically $ STM.modifyTVar var ( set nssLastMargin newMargin
                                    . set nssLastLocalTime realTime)

ntpSettings
    :: (MonadIO m, WithLogger m)
    => NtpSlottingVar -> NtpClientSettings m
ntpSettings var = NtpClientSettings
    { -- list of servers addresses
      ntpServers         = [ "time.windows.com"
                           , "clock.isc.org"
                           , "ntp5.stratum2.ru"]
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
    , ntpMeanSelection   = \l -> let len = length l in sort l !! ((len - 1) `div` 2)
    }
