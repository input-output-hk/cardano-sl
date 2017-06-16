{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeFamilies        #-}

-- | 'MonadSlots' implementation which uses Ntp servers.

module Pos.Slotting.Ntp
       ( NtpSlottingState
       , NtpSlottingVar
       , askNtpSlotting
       , askFullNtpSlotting
       , mkNtpSlottingVar
       , SlotsRedirect
       , runSlotsRedirect
       ) where

import           Universum

import qualified Control.Concurrent.STM       as STM
import           Control.Lens                 (makeLenses)
import           Control.Monad.Trans.Control  (MonadBaseControl)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Data.Coerce                  (coerce)
import           Data.List                    ((!!))
import           Data.Time.Units              (Microsecond, convertUnit)
import qualified Ether
import           Formatting                   (int, sformat, shown, stext, (%))
import           Mockable                     (Catch, CurrentTime, Delay, Fork, Mockables,
                                               Throw, currentTime, delay)
import           NTP.Client                   (NtpClientSettings (..), ntpSingleShot,
                                               startNtpClient)
import           NTP.Example                  ()
import           Serokell.Util                (sec)
import           System.Wlog                  (WithLogger, logDebug, logInfo, logWarning)

import qualified Pos.Core.Constants           as C
import           Pos.Core.Slotting            (flattenEpochIndex, unflattenSlotId)
import           Pos.Core.Types               (EpochIndex, SlotId (..), Timestamp (..),
                                               mkLocalSlotIndex)
import           Pos.Util.Util                (leftToPanic)

import           Pos.Slotting.Class           (MonadSlots (..))
import qualified Pos.Slotting.Constants       as C
import           Pos.Slotting.MemState.Class  (MonadSlotsData (..))
import           Pos.Slotting.Types           (EpochSlottingData (..), SlottingData (..))

----------------------------------------------------------------------------
-- State
----------------------------------------------------------------------------

-- | Data needed for the slotting algorithm to work.
data NtpSlottingState = NtpSlottingState
    {
    -- | Slot which was returned from getCurrentSlot last time.
       _nssLastSlot     :: !SlotId
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

-- | Monad which implements NTP-based solution for slotting.
-- Flag means whether to use real NTP servers or rely on local time.
type MonadNtpSlotting = Ether.MonadReader' (Bool, NtpSlottingVar)

askNtpSlotting :: MonadNtpSlotting m => m NtpSlottingVar
askNtpSlotting = Ether.asks' @(Bool, NtpSlottingVar) snd

askFullNtpSlotting :: MonadNtpSlotting m => m (Bool, NtpSlottingVar)
askFullNtpSlotting = Ether.ask'

----------------------------------------------------------------------------
-- MonadSlots implementation
----------------------------------------------------------------------------

type SlottingConstraint m =
    ( MonadIO m
    , MonadBaseControl IO m
    , WithLogger m
    , MonadSlotsData m
    , MonadCatch m
    , MonadMask m
    , Mockables m
        [ Fork
        , Throw
        , Catch
        , Delay
        , CurrentTime
        ]
    )

data SlotsRedirectTag

type SlotsRedirect =
    Ether.TaggedTrans SlotsRedirectTag IdentityT

runSlotsRedirect :: SlotsRedirect m a -> m a
runSlotsRedirect = coerce

instance
    (MonadNtpSlotting m, SlottingConstraint m, MonadIO m, t ~ IdentityT) =>
         MonadSlots (Ether.TaggedTrans SlotsRedirectTag t m)
  where
    getCurrentSlot =
        ifNtpUsed ntpGetCurrentSlot simpleGetCurrentSlot
    getCurrentSlotBlocking =
        ifNtpUsed ntpGetCurrentSlotBlocking simpleGetCurrentSlotBlocking
    getCurrentSlotInaccurate =
        ifNtpUsed ntpGetCurrentSlotInaccurate simpleGetCurrentSlotInaccurate
    currentTimeSlotting =
        ifNtpUsed ntpCurrentTime simpleCurrentTimeSlotting
    slottingWorkers = [ntpSyncWorker]

ifNtpUsed :: MonadNtpSlotting m => m a -> m a -> m a
ifNtpUsed t f = Ether.asks' @(Bool, NtpSlottingVar) fst >>= bool f t

----------------------------------------------------------------------------
-- Getting current slot
----------------------------------------------------------------------------

data SlotStatus
    = CantTrust Text                    -- ^ We can't trust local time.
    | OutdatedSlottingData !EpochIndex  -- ^ We don't know recent
                                        -- slotting data, last known
                                        -- penult epoch is attached.
    | CurrentSlot !SlotId               -- ^ Slot is calculated successfully.

ntpGetCurrentSlot
    :: (SlottingConstraint m, MonadNtpSlotting m)
    => m (Maybe SlotId)
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

ntpGetCurrentSlotInaccurate
    :: (SlottingConstraint m, MonadNtpSlotting m)
    => m SlotId
ntpGetCurrentSlotInaccurate = do
    res <- ntpGetCurrentSlotImpl
    case res of
        CurrentSlot slot -> pure slot
        CantTrust _        -> do
            var <- askNtpSlotting
            _nssLastSlot <$> atomically (STM.readTVar var)
        OutdatedSlottingData penult ->
            ntpCurrentTime >>= approxSlotUsingOutdated penult

ntpGetCurrentSlotImpl
    :: (SlottingConstraint m, MonadNtpSlotting m)
    => m SlotStatus
ntpGetCurrentSlotImpl = do
    var <- askNtpSlotting
    NtpSlottingState {..} <- atomically $ STM.readTVar var
    t <- Timestamp . (+ _nssLastMargin) <$> currentTime
    case canWeTrustLocalTime _nssLastLocalTime t of
      Nothing -> do
          penult <- sdPenultEpoch <$> getSlottingData
          res <- fmap (max _nssLastSlot) <$> getCurrentSlotDo t
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

ntpGetCurrentSlotBlocking
    :: (SlottingConstraint m, MonadNtpSlotting m)
    => m SlotId
ntpGetCurrentSlotBlocking = ntpGetCurrentSlotImpl >>= \case
    CantTrust _ -> do
        delay C.ntpPollDelay
        ntpGetCurrentSlotBlocking
    OutdatedSlottingData penult -> do
        waitPenultEpochEquals (penult + 1)
        ntpGetCurrentSlotBlocking
    CurrentSlot slot -> pure slot

ntpCurrentTime
    :: (SlottingConstraint m, MonadNtpSlotting m)
    => m Timestamp
ntpCurrentTime = do
    var <- askNtpSlotting
    lastMargin <- view nssLastMargin <$> atomically (STM.readTVar var)
    Timestamp . (+ lastMargin) <$> currentTime

-- Independent of slotting algorithm functions
approxSlotUsingOutdated :: SlottingConstraint m => EpochIndex -> Timestamp -> m SlotId
approxSlotUsingOutdated penult t = do
    SlottingData {..} <- getSlottingData
    pure $
        if | t < esdStart sdLast -> SlotId (penult + 1) minBound
           | otherwise           -> outdatedEpoch t (penult + 1) sdLast
  where
    outdatedEpoch (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp esdStart in
        unflattenSlotId $
        flattenEpochIndex epoch + fromIntegral ((curTime - start) `div` duration)

getCurrentSlotDo
    :: SlottingConstraint m
    => Timestamp -> m (Maybe SlotId)
getCurrentSlotDo approxCurTime = do
    SlottingData {..} <- getSlottingData
    let tryEpoch = computeSlotUsingEpoch approxCurTime
    let penultRes = tryEpoch sdPenultEpoch sdPenult
    let lastRes = tryEpoch (succ sdPenultEpoch) sdLast
    return $ penultRes <|> lastRes

computeSlotUsingEpoch
    :: Timestamp
    -> EpochIndex
    -> EpochSlottingData
    -> Maybe SlotId
computeSlotUsingEpoch (Timestamp curTime) epoch EpochSlottingData {..}
    | curTime < start = Nothing
    | curTime < start + duration * C.epochSlots = Just $ SlotId epoch localSlot
    | otherwise = Nothing
  where
    localSlotNumeric = fromIntegral $ (curTime - start) `div` duration
    localSlot =
        leftToPanic "computeSlotUsingEpoch: " $
        mkLocalSlotIndex localSlotNumeric
    duration = convertUnit esdSlotDuration
    start = getTimestamp esdStart

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

mkNtpSlottingVar
    :: ( MonadIO m
       , MonadMask m
       , MonadBaseControl IO m
       , WithLogger m
       , MonadMask m
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
-- Workers
----------------------------------------------------------------------------

-- Worker for synchronization of local time and global time.
ntpSyncWorker
    :: (SlottingConstraint m, MonadNtpSlotting m)
    => m ()
ntpSyncWorker =
    ifNtpUsed (askNtpSlotting >>= void . startNtpClient . ntpSettings) pass

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

----------------------------------------------------------------------------
-- Simple Slotting
----------------------------------------------------------------------------

simpleGetCurrentSlot :: SlottingConstraint m => m (Maybe SlotId)
simpleGetCurrentSlot = simpleCurrentTimeSlotting >>= getCurrentSlotDo

simpleGetCurrentSlotBlocking :: SlottingConstraint m => m SlotId
simpleGetCurrentSlotBlocking = do
    penult <- sdPenultEpoch <$> getSlottingData
    simpleGetCurrentSlot >>= \case
        Just slot -> pure slot
        Nothing -> do
            waitPenultEpochEquals (penult + 1)
            simpleGetCurrentSlotBlocking

simpleGetCurrentSlotInaccurate :: SlottingConstraint m => m SlotId
simpleGetCurrentSlotInaccurate = do
    penult <- sdPenultEpoch <$> getSlottingData
    simpleGetCurrentSlot >>= \case
        Just slot -> pure slot
        Nothing -> simpleCurrentTimeSlotting >>= approxSlotUsingOutdated penult

simpleCurrentTimeSlotting :: SlottingConstraint m => m Timestamp
simpleCurrentTimeSlotting = Timestamp <$> currentTime
