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
       , runNtpSlotting
       , runNtpSlottingFromVar
       ) where

import           Control.Concurrent.STM      (TVar)
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
import           Data.Time.Units             (Microsecond)
import           Formatting                  (int, sformat, (%))
import           Mockable                    (Catch, ChannelT, Counter, CurrentTime,
                                              Delay, Distribution, Fork, Gauge, MFunctor',
                                              Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              Throw, liftMockableWrappedM)
import           NTP.Client                  (NtpClientSettings (..), startNtpClient)
import           NTP.Example                 ()
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           System.Wlog                 (logNotice, modifyLoggerName)
import           System.Wlog                 (WithLogger, logDebug)
import           Universum


import qualified Pos.Constants               as C
import           Pos.Context                 (NodeContext (..), getNodeContext)
import           Pos.Context.Class           (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import           Pos.Slotting.Class          (MonadSlots (..), MonadSlotsData (..))
import           Pos.Slotting.Util           (onNewSlotWithLogging)
import           Pos.Types                   (SlotId, slotIdF)
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
    , _nssLastLocalTime :: !Microsecond
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

               , MonadDB σ
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

type SlottingConstraint ssc m =
    ( MonadIO m
    , WithLogger m
    , MonadSlotsData m
    , Mockable Fork m
    , Mockable Throw m
    , Mockable Catch m
    , MonadCatch m
    , Mockable Delay m
    , WithNodeContext ssc m
    )

instance SlottingConstraint ssc m =>
         MonadSlots (NtpSlotting m) where
    getCurrentSlot = notImplemented
    slottingWorkers = [ntpSyncWorker, newSlotWorker]

-- instance (Mockable CurrentTime m, MonadIO m) =>
--          MonadSlots (ContextHolder ssc m) where
--     getSystemStartTime = ContextHolder $ asks ncSystemStart

--     getCurrentTime = do
--         lastMargin <- readNtpMargin
--         Timestamp . (+ lastMargin) <$> currentTime

--     getCurrentSlot = do
--         lastSlot <- readNtpLastSlot
--         ntpData <- readNtpData
--         getCurrentSlotUsingNtp lastSlot ntpData

    -- getSlotDuration = pure genesisSlotDuration

-- getCurrentSlotUsingNtp :: (MonadSlots m, Mockable CurrentTime m)
--                        => SlotId -> (Microsecond, Microsecond) -> m SlotId
-- getCurrentSlotUsingNtp lastSlot (margin, measTime) = do
--     t <- (+ margin) <$> currentTime
--     canTrust <- canWeTrustLocalTime t
--     slotDuration <- getSlotDuration
--     if canTrust then
--         max lastSlot . f (convertUnit slotDuration) <$>
--             ((t -) . getTimestamp <$> getSystemStartTime)
--     else pure lastSlot
--   where
--     f :: Microsecond -> Microsecond -> SlotId
--     f slotDuration diff
--         | diff < 0 = SlotId 0 0
--         | otherwise = unflattenSlotId (fromIntegral $ diff `div` slotDuration)
--     -- We can trust getCurrentTime if it isn't bigger than:
--     -- time for which we got margin (in last time) + NTP delay (+ some eps, for safety)
--     canWeTrustLocalTime t =
--         pure $ t <= measTime + ntpPollDelay + ntpMaxError

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

runNtpSlotting :: (Mockable CurrentTime m) => NtpSlotting m a -> m a
runNtpSlotting = notImplemented
    -- slottingStateVar <- do
    --     _ssNtpData <- (0,) <$> currentTime
    --     -- current time isn't quite validly, but it doesn't matter
    --     let _ssNtpLastSlot = unflattenSlotId 0
    --     liftIO $ newTVarIO SlottingState{..}

runNtpSlottingFromVar :: NtpSlottingVar -> NtpSlotting m a -> m a
runNtpSlottingFromVar var = usingReaderT var . getNtpSlotting

----------------------------------------------------------------------------
-- Something
----------------------------------------------------------------------------

-- readNtpLastSlot :: (MonadIO m, WithNodeContext ssc m) => m SlotId
-- readNtpLastSlot = do
--     nc <- getNodeContext
--     atomically $ view ssNtpLastSlot <$> STM.readTVar (ncSlottingState nc)

-- readNtpMargin :: (MonadIO m, WithNodeContext ssc m) => m Microsecond
-- readNtpMargin = do
--     nc <- getNodeContext
--     atomically $ fst . view ssNtpData <$> STM.readTVar (ncSlottingState nc)

-- readNtpData
--     :: (MonadIO m, WithNodeContext ssc m)
--     => m (Microsecond, Microsecond)
-- readNtpData = do
--     nc <- getNodeContext
--     atomically $ view ssNtpData <$> STM.readTVar (ncSlottingState nc)

----------------------------------------------------------------------------
-- Workers
----------------------------------------------------------------------------

-- This worker updates last slot (WTF).
newSlotWorker
    :: SlottingConstraint ssc m
    => NtpSlotting m ()
newSlotWorker =
    onNewSlotWithLogging True $ \slotId -> do
        modifyLoggerName (<> "slotting") $
            logNotice $ sformat ("New slot has just started: " %slotIdF) slotId
        setNtpLastSlot slotId
  where
    setNtpLastSlot slotId = do
        var <- NtpSlotting ask
        atomically $ STM.modifyTVar (var) (nssLastSlot %~ max slotId)

-- Worker for synchronization of local time and global time.
ntpSyncWorker
    :: SlottingConstraint ssc m
    => NtpSlotting m ()
ntpSyncWorker = NtpSlotting ask >>= void . startNtpClient . ntpSettings

ntpHandlerDo
    :: (MonadIO m, WithLogger m)
    => NtpSlottingVar -> (Microsecond, Microsecond) -> m ()
ntpHandlerDo var (newMargin, transmitTime) = do
    logDebug $ sformat ("Callback on new margin: "%int% " mcs") newMargin
    let realTime = transmitTime + newMargin
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


----------------------------------------------------------------------------
-- Something else
----------------------------------------------------------------------------

-- waitSystemStart :: WorkMode ssc m => m ()
-- waitSystemStart = do
--     unless isDevelopment $ delay (ntpResponseTimeout + ntpMaxError)
--     margin <- readNtpMargin
--     Timestamp start <- ncSystemStart . ncNodeParams <$> getNodeContext
--     cur <- (+ margin) <$> currentTime
--     let waitPeriod = start - cur
--     logInfo $ sformat ("Waiting "%int%" seconds for system start") $
--         waitPeriod `div` sec 1
--     when (cur < start) $ delay waitPeriod
