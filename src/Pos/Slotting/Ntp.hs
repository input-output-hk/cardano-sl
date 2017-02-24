{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | 'MonadSlots' implementation which uses Ntp servers.

module Pos.Slotting.Ntp
       ( NtpSlottingState
       , NtpSlottingVar

       , NtpSlotting (..)
       , mkNtpSlottingVar
       , runNtpSlotting
       ) where

import           Control.Concurrent.STM      (TVar, newTVarIO)
import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso, makeLenses)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Data.List                   ((!!))
import           Data.Time.Units             (Microsecond, convertUnit)
import           Formatting                  (int, sformat, (%))
import           Mockable                    (Catch, ChannelT, Counter, CurrentTime,
                                              Delay, Distribution, Fork, Gauge, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              Throw, currentTime, delay,
                                              liftMockableWrappedM)
import           NTP.Client                  (NtpClientSettings (..), ntpSingleShot,
                                              startNtpClient)
import           NTP.Example                 ()
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName, WithLogger, logDebug,
                                              logInfo)
import           Universum


import qualified Pos.Constants               as C
import           Pos.Context.Class           (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Slotting.Class          (MonadSlots (..), MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..), SlottingData (..))
import           Pos.Types                   (EpochIndex, SlotId (..), Timestamp (..),
                                              flattenEpochIndex, unflattenSlotId)
import           Pos.Util.JsonLog            (MonadJL)

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
newtype NtpSlotting m a = NtpSlotting
    { getNtpSlotting :: ReaderT NtpSlottingVar m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadTrans
               , MonadIO
               , MonadFix

               , MonadThrow
               , MonadCatch
               , MonadMask

               , MonadBase base

               , HasLoggerName
               , CanLog

               , MonadDB
               , WithNodeContext ssc
               , MonadJL
               , MonadSlotsData
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (NtpSlotting m) = ThreadId m
type instance Promise (NtpSlotting m) = Promise m
type instance SharedAtomicT (NtpSlotting m) = SharedAtomicT m
type instance Counter (NtpSlotting m) = Counter m
type instance Distribution (NtpSlotting m) = Distribution m
type instance SharedExclusiveT (NtpSlotting m) = SharedExclusiveT m
type instance Gauge (NtpSlotting m) = Gauge m
type instance ChannelT (NtpSlotting m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (NtpSlotting m) (ReaderT NtpSlottingVar m)
         , MFunctor' d (ReaderT NtpSlottingVar m) m
         ) => Mockable d (NtpSlotting m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (NtpSlotting m) where
    type UnwrappedM (NtpSlotting m) = ReaderT NtpSlottingVar m
    _WrappedM = iso getNtpSlotting NtpSlotting

instance MonadTransControl NtpSlotting where
    type StT (NtpSlotting) a = StT (ReaderT NtpSlottingVar) a
    liftWith = defaultLiftWith NtpSlotting getNtpSlotting
    restoreT = defaultRestoreT NtpSlotting

instance MonadBaseControl IO m => MonadBaseControl IO (NtpSlotting m) where
    type StM (NtpSlotting m) a = ComposeSt NtpSlotting m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

----------------------------------------------------------------------------
-- MonadSlots implementation
----------------------------------------------------------------------------

type SlottingConstraint m =
    ( MonadIO m
    , WithLogger m
    , MonadSlotsData m
    , Mockable Fork m
    , Mockable Throw m
    , Mockable Catch m
    , MonadCatch m
    , Mockable Delay m
    , Mockable CurrentTime m
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
    = CantTrust  -- ^ We can't trust local time.
    | OutdatedSlottingData !EpochIndex  -- ^ We don't know recent
                                        -- slotting data, last known
                                        -- penult epoch is attached.
    | CurrentSlot !SlotId -- ^ Slot is calculated successfully.

ntpGetCurrentSlot :: SlottingConstraint m => NtpSlotting m (Maybe SlotId)
ntpGetCurrentSlot = do
    ntpGetCurrentSlotImpl >>= \case
        CurrentSlot slot ->
            return (Just slot)
        OutdatedSlottingData i -> do
            logDebug $ sformat
                ("Can't get current slot, because slotting data"%
                 " is outdated. Last known penult epoch = "%int)
                i
            return Nothing
        CantTrust -> do
            logDebug
                "Can't get current slot, because we can't trust local time"
            return Nothing

ntpGetCurrentSlotInaccurate :: SlottingConstraint m => NtpSlotting m SlotId
ntpGetCurrentSlotInaccurate = do
    res <- ntpGetCurrentSlotImpl
    case res of
        CurrentSlot slot -> pure slot
        CantTrust        -> do
            var <- NtpSlotting ask
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
    var <- NtpSlotting ask
    NtpSlottingState {..} <- atomically $ STM.readTVar var
    t <- Timestamp . (+ _nssLastMargin) <$> currentTime
    if | canWeTrustLocalTime _nssLastLocalTime t ->
           do penult <- sdPenultEpoch <$> getSlottingData
              res <- fmap (max _nssLastSlot) <$> ntpGetCurrentSlotDo t
              let setLastSlot s =
                      atomically $ STM.modifyTVar' var (nssLastSlot %~ max s)
              whenJust res setLastSlot
              pure $ maybe (OutdatedSlottingData penult) CurrentSlot res
       | otherwise -> return CantTrust
  where
    -- We can trust getCurrentTime if it is:
    -- • not bigger than 'time for which we got margin (last time)
    --   + NTP delay (+ some eps, for safety)'
    -- • not less than 'last time - some eps'
    canWeTrustLocalTime :: Timestamp -> Timestamp -> Bool
    canWeTrustLocalTime (Timestamp lastLocalTime) (Timestamp t) =
        t <= lastLocalTime + C.ntpPollDelay + C.ntpMaxError &&
        lastLocalTime - C.ntpMaxError <= t

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
        Just $ SlotId epoch (fromIntegral $ (curTime - start) `div` duration)
    | otherwise = Nothing
  where
    duration = convertUnit esdSlotDuration
    start = getTimestamp esdStart

ntpGetCurrentSlotBlocking :: SlottingConstraint m => NtpSlotting m SlotId
ntpGetCurrentSlotBlocking = do
    slotSt <- ntpGetCurrentSlotImpl
    case slotSt of
        CantTrust -> do
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
    var <- NtpSlotting ask
    lastMargin <- view nssLastMargin <$> atomically (STM.readTVar var)
    Timestamp . (+ lastMargin) <$> currentTime

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

mkNtpSlottingVar
    :: ( MonadIO m
       , Mockable CurrentTime m
       , WithLogger m
       , Mockable Delay m
       , Mockable Fork m
       , Mockable Throw m
       , Mockable Catch m
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
runNtpSlotting var = usingReaderT var . getNtpSlotting

----------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------

-- Worker for synchronization of local time and global time.
ntpSyncWorker
    :: SlottingConstraint m
    => NtpSlotting m ()
ntpSyncWorker = NtpSlotting ask >>= void . startNtpClient . ntpSettings

ntpHandlerDo
    :: (MonadIO m, WithLogger m)
    => NtpSlottingVar -> (Microsecond, Microsecond) -> m ()
ntpHandlerDo var (newMargin, transmitTime) = do
    logDebug $ sformat ("Callback on new margin: "%int% " mcs") newMargin
    let realTime = Timestamp $ transmitTime + newMargin
    atomically $ STM.modifyTVar var ( set nssLastMargin newMargin
                                    . set nssLastLocalTime realTime)

ntpSettings :: NtpSlottingVar -> NtpClientSettings
ntpSettings var = NtpClientSettings
        { -- list of servers addresses
          ntpServers         = [ "pool.ntp.org"
                               , "time.windows.com"
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
