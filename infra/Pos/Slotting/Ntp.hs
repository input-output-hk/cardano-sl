{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'MonadSlots' implementation which uses Ntp servers.

module Pos.Slotting.Ntp
       ( NtpSlottingState
       , NtpSlottingVar

       , NtpSlotting
       , MonadNtpSlotting(..)
       , mkNtpSlottingVar
       , runNtpSlotting
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (makeLenses)
import qualified Control.Monad.Ether.Implicit as Ether
import           Control.Monad.Trans.Class  (MonadTrans)
import           Data.List                   ((!!))
import           Data.Time.Units             (Microsecond, convertUnit)
import           Formatting                  (int, sformat, shown, stext, (%))
import           Mockable                    (Catch, Mockables, Fork, Delay,
                                              CurrentTime, Throw, currentTime, delay)
import           NTP.Client                  (NtpClientSettings (..), ntpSingleShot,
                                              startNtpClient)
import           NTP.Example                 ()
import           System.Wlog                 (WithLogger, logDebug,
                                              logInfo, logWarning)
import           Universum

import qualified Pos.Core.Constants          as C
import           Pos.Core.Slotting           (flattenEpochIndex, unflattenSlotId)
import           Pos.Core.Types              (EpochIndex, SlotId (..), Timestamp (..))

import           Pos.Slotting.Class          (MonadSlots (..))
import qualified Pos.Slotting.Constants      as C
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..), SlottingData (..))

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

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | Monad transformer which implements NTP-based solution for slotting.
type NtpSlotting = Ether.ReaderT NtpSlottingVar

class Monad m => MonadNtpSlotting m where
  askNtpSlotting :: m NtpSlottingVar

  default askNtpSlotting
    :: (MonadNtpSlotting m', MonadTrans t, m ~ t m') => m NtpSlottingVar
  askNtpSlotting = lift askNtpSlotting

instance {-# OVERLAPPABLE #-}
  (MonadNtpSlotting m, MonadTrans t, Monad (t m)) =>
  MonadNtpSlotting (t m)

instance Monad m => MonadNtpSlotting (NtpSlotting m) where
  askNtpSlotting = Ether.ask

----------------------------------------------------------------------------
-- MonadSlots implementation
----------------------------------------------------------------------------

type SlottingConstraint m =
    ( MonadIO m
    , WithLogger m
    , MonadSlotsData m
    , MonadCatch m
    , Mockables m
        [ Fork
        , Throw
        , Catch
        , Delay
        , CurrentTime
        ]
    )

instance SlottingConstraint m =>
         MonadSlots (NtpSlotting m) where
    getCurrentSlot = ntpGetCurrentSlot
    getCurrentSlotBlocking = ntpGetCurrentSlotBlocking
    getCurrentSlotInaccurate = ntpGetCurrentSlotInaccurate
    currentTimeSlotting = ntpCurrentTime
    slottingWorkers = [ntpSyncWorker]

----------------------------------------------------------------------------
-- Getting current slot
----------------------------------------------------------------------------

data SlotStatus
    = CantTrust Text                    -- ^ We can't trust local time.
    | OutdatedSlottingData !EpochIndex  -- ^ We don't know recent
                                        -- slotting data, last known
                                        -- penult epoch is attached.
    | CurrentSlot !SlotId               -- ^ Slot is calculated successfully.

ntpGetCurrentSlot :: SlottingConstraint m => NtpSlotting m (Maybe SlotId)
ntpGetCurrentSlot = ntpGetCurrentSlotImpl >>= \case
    CurrentSlot slot -> pure $ Just slot
    OutdatedSlottingData i -> do
        logWarning $ sformat
            ("Can't get current slot, because slotting data"%
             " is outdated. Last known penult epoch = "%int)
            i
        Nothing <$ printSlottingData
    CantTrust t -> do
        logWarning $
            "Can't get current slot, because we can't trust local time, details: " <> t
        Nothing <$ printSlottingData
  where
    printSlottingData = do
        sd <- getSlottingData
        logWarning $ "Slotting data: " <> show sd

ntpGetCurrentSlotInaccurate :: SlottingConstraint m => NtpSlotting m SlotId
ntpGetCurrentSlotInaccurate = do
    res <- ntpGetCurrentSlotImpl
    case res of
        CurrentSlot slot -> pure slot
        CantTrust _        -> do
            var <- askNtpSlotting
            _nssLastSlot <$> atomically (STM.readTVar var)
        OutdatedSlottingData penult -> do
            t <- ntpCurrentTime
            SlottingData {..} <- getSlottingData
            pure $
                if | t < esdStart sdLast -> SlotId (penult + 1) 0
                   | otherwise ->           outdatedEpoch t (penult + 1) sdLast
  where
    outdatedEpoch (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp esdStart in
        unflattenSlotId $
        flattenEpochIndex epoch + fromIntegral ((curTime - start) `div` duration)

ntpGetCurrentSlotImpl :: SlottingConstraint m => NtpSlotting m SlotStatus
ntpGetCurrentSlotImpl = do
    var <- askNtpSlotting
    NtpSlottingState {..} <- atomically $ STM.readTVar var
    t <- Timestamp . (+ _nssLastMargin) <$> currentTime
    case canWeTrustLocalTime _nssLastLocalTime t of
      Nothing -> do
          penult <- sdPenultEpoch <$> getSlottingData
          res <- fmap (max _nssLastSlot) <$> ntpGetCurrentSlotDo t
          let setLastSlot s =
                  atomically $ STM.modifyTVar' var (nssLastSlot %~ max s)
          whenJust res setLastSlot
          pure $ maybe (OutdatedSlottingData penult) CurrentSlot res
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

ntpGetCurrentSlotDo
    :: SlottingConstraint m
    => Timestamp -> NtpSlotting m (Maybe SlotId)
ntpGetCurrentSlotDo approxCurTime = do
    SlottingData {..} <- getSlottingData
    let tryEpoch = ntpGetCurrentSlotTryEpoch approxCurTime
    let penultRes = tryEpoch sdPenultEpoch sdPenult
    let lastRes = tryEpoch (succ sdPenultEpoch) sdLast
    return $ penultRes <|> lastRes

ntpGetCurrentSlotTryEpoch
    :: Timestamp
    -> EpochIndex
    -> EpochSlottingData
    -> Maybe SlotId
ntpGetCurrentSlotTryEpoch (Timestamp curTime) epoch EpochSlottingData {..}
    | curTime < start = Nothing
    | curTime < start + duration * C.epochSlots =
        Just $ SlotId epoch $ fromIntegral $ (curTime - start) `div` duration
    | otherwise = Nothing
  where
    duration = convertUnit esdSlotDuration
    start = getTimestamp esdStart

ntpGetCurrentSlotBlocking :: SlottingConstraint m => NtpSlotting m SlotId
ntpGetCurrentSlotBlocking = ntpGetCurrentSlotImpl >>= \case
    CantTrust _ -> do
        delay C.ntpPollDelay
        ntpGetCurrentSlotBlocking
    OutdatedSlottingData penult -> do
        waitPenultEpochEquals (penult + 1)
        ntpGetCurrentSlotBlocking
    CurrentSlot slot -> pure slot

ntpCurrentTime
    :: SlottingConstraint m
    => NtpSlotting m Timestamp
ntpCurrentTime = do
    var <- askNtpSlotting
    lastMargin <- view nssLastMargin <$> atomically (STM.readTVar var)
    Timestamp . (+ lastMargin) <$> currentTime

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

mkNtpSlottingVar
    :: ( MonadIO m
       , WithLogger m
       , Mockables m
        [ CurrentTime
        , Delay
        , Fork
        , Throw
        , Catch
        ]
       )
    => m NtpSlottingVar
mkNtpSlottingVar = do
    let _nssLastMargin = 0
    _nssLastLocalTime <- Timestamp <$> currentTime
    -- current time isn't quite valid value, but it doesn't matter (@pva701)
    let _nssLastSlot = unflattenSlotId 0
    res <- liftIO $ newTVarIO NtpSlottingState {..}
    let settings = ntpSettings res
    res <$ singleShot settings
  where
    singleShot settings = unless C.isDevelopment $ do
        logInfo $ "Waiting for response from NTP servers"
        ntpSingleShot settings
        delay C.ntpMaxError

runNtpSlotting :: NtpSlottingVar -> NtpSlotting m a -> m a
runNtpSlotting = flip Ether.runReaderT

----------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------

-- Worker for synchronization of local time and global time.
ntpSyncWorker
    :: SlottingConstraint m
    => NtpSlotting m ()
ntpSyncWorker = askNtpSlotting >>= void . startNtpClient . ntpSettings

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
