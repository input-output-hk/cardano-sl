{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of WithNodeContext.

module Pos.Context.Holder
       ( ContextHolder (..)
       , runContextHolder
       ) where

import           Control.Concurrent.MVar     (withMVar)
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow, catchAll)
import           Control.Monad.Morph         (hoist)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadResponse (..),
                                              MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)
import           Data.Time.Units             (Microsecond)

import           Formatting                  (sformat, shown, (%))
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName, WithLogger,
                                              logWarning)
import           Universum

import           Pos.Constants               (ntpMaxError, ntpPollDelay, slotDuration)
import           Pos.Context.Class           (WithNodeContext (..), readNtpLastSlot,
                                              readNtpMargin, readNtpTimestamp)
import           Pos.Context.Context         (NodeContext (..))
import           Pos.DB                      (MonadDB)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Txp.Class               (MonadTxpLD)
import           Pos.Types                   (SlotId, Timestamp (..), unflattenSlotId)
import           Pos.Util.JsonLog            (MonadJL (..), appendJL)

-- | Wrapper for monadic action which brings 'NodeContext'.
newtype ContextHolder ssc m a = ContextHolder
    { getContextHolder :: ReaderT (NodeContext ssc) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog, MonadDB ssc,
                MonadTxpLD ssc, MonadDialog s p)

-- | Run 'ContextHolder' action.
runContextHolder :: NodeContext ssc -> ContextHolder ssc m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

instance Monad m => WrappedM (ContextHolder ssc m) where
    type UnwrappedM (ContextHolder ssc m) = ReaderT (NodeContext ssc) m
    _WrappedM = iso getContextHolder ContextHolder

instance MonadBase IO m => MonadBase IO (ContextHolder ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (ContextHolder ssc) where
    type StT (ContextHolder ssc) a = StT (ReaderT (NodeContext ssc)) a
    liftWith = defaultLiftWith ContextHolder getContextHolder
    restoreT = defaultRestoreT ContextHolder

instance MonadBaseControl IO m => MonadBaseControl IO (ContextHolder ssc m) where
    type StM (ContextHolder ssc m) a = ComposeSt (ContextHolder ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (ContextHolder ssc m) = ThreadId m

instance MonadTransfer s m => MonadTransfer s (ContextHolder ssc m)

instance MonadResponse s m => MonadResponse s (ContextHolder ssc m) where
    replyRaw dat = ContextHolder $ replyRaw (hoist getContextHolder dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance Monad m => WithNodeContext ssc (ContextHolder ssc m) where
    getNodeContext = ContextHolder ask

instance (MonadTimed m, Monad m, MonadIO m) =>
         MonadSlots (ContextHolder ssc m) where
    getSystemStartTime = ContextHolder $ asks ncSystemStart

    getCurrentTime = do
        lastMargin <- readNtpMargin
        Timestamp . (+ lastMargin) <$> currentTime

    getCurrentSlot = do
        lastSlot <- readNtpLastSlot
        t <- getTimestamp <$> getCurrentTime
        canTrust <- canWeTrustTime t
        if canTrust then
            max lastSlot . f  <$>
                ((t -) . getTimestamp <$> getSystemStartTime)
        else pure lastSlot
      where
        f :: Microsecond -> SlotId
        f t = unflattenSlotId (fromIntegral $ t `div` slotDuration)
        canWeTrustTime t = do
            measTime <- readNtpTimestamp
            return $ t <= measTime + ntpPollDelay + ntpMaxError

instance (MonadIO m, MonadCatch m, WithLogger m) => MonadJL (ContextHolder ssc m) where
    jlLog ev = ContextHolder (asks ncJLFile) >>= maybe (pure ()) doLog
      where
        doLog logFileMV =
          (liftIO . withMVar logFileMV $ flip appendJL ev)
            `catchAll` \e -> logWarning $ sformat ("Can't write to json log: " % shown) e
