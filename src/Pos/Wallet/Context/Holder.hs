{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of `WithWalletContext`

module Pos.Wallet.Context.Holder
       ( ContextHolder (..)
       , runContextHolder
       ) where

import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)
import           Data.Time.Units             (Microsecond)

import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Control.TimeWarp.Rpc        (MonadDialog, MonadTransfer (..))
import           Control.TimeWarp.Timed      (MonadTimed (..), ThreadId)

import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Constants               (ntpMaxError, ntpPollDelay, slotDuration)
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Types                   (Timestamp (..), SlotId, unflattenSlotId)
import           Pos.Wallet.Context.Class    (WithWalletContext (..), readNtpLastSlot,
                                              readNtpMargin, readNtpTimestamp)
import           Pos.Wallet.Context.Context  (WalletContext (..))

-- | Wrapper for monadic action which brings 'WalletContext'.
newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT WalletContext m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog, MonadDialog s p)

-- | Run 'ContextHolder' action.
runContextHolder :: WalletContext -> ContextHolder m a -> m a
runContextHolder ctx = flip runReaderT ctx . getContextHolder

instance Monad m => WrappedM (ContextHolder m) where
    type UnwrappedM (ContextHolder m) = ReaderT WalletContext m
    _WrappedM = iso getContextHolder ContextHolder

instance MonadBase IO m => MonadBase IO (ContextHolder m) where
    liftBase = lift . liftBase

instance MonadTransControl ContextHolder where
    type StT ContextHolder a = StT (ReaderT WalletContext) a
    liftWith = defaultLiftWith ContextHolder getContextHolder
    restoreT = defaultRestoreT ContextHolder

instance MonadBaseControl IO m => MonadBaseControl IO (ContextHolder m) where
    type StM (ContextHolder m) a = ComposeSt ContextHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (ContextHolder m) = ThreadId m

instance MonadTransfer s m => MonadTransfer s (ContextHolder m)

instance Monad m => WithWalletContext (ContextHolder m) where
    getWalletContext = ContextHolder ask

instance (MonadTimed m, Monad m, MonadIO m) =>
         MonadSlots (ContextHolder m) where
    getSystemStartTime = ContextHolder $ asks wcSystemStart

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
