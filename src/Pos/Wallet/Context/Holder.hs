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
import           Control.Monad.Fix           (MonadFix)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Control.Monad.Trans.Control (ComposeSt, MonadBaseControl (..),
                                              MonadTransControl (..), StM,
                                              defaultLiftBaseWith, defaultLiftWith,
                                              defaultRestoreM, defaultRestoreT)
import           Mockable                    (ChannelT, Counter, Distribution, Gauge,
                                              MFunctor', Mockable (liftMockable), Promise,
                                              SharedAtomicT, SharedExclusiveT, ThreadId,
                                              liftMockableWrappedM)
import           Serokell.Util.Lens          (WrappedM (..))
import           System.Wlog                 (CanLog, HasLoggerName)
import           Universum

import           Pos.Reporting.MemState      (MonadReportingMem (..),
                                              ReportingContext (..))
import           Pos.Wallet.Context.Class    (WithWalletContext (..))
import           Pos.Wallet.Context.Context  (WalletContext (..))

-- | Wrapper for monadic action which brings 'WalletContext'.
newtype ContextHolder m a = ContextHolder
    { getContextHolder :: ReaderT WalletContext m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadFix,
                MonadThrow, MonadCatch, MonadMask, MonadIO, MonadFail,
                HasLoggerName, CanLog)

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
type instance Promise (ContextHolder m) = Promise m
type instance SharedAtomicT (ContextHolder m) = SharedAtomicT m
type instance Counter (ContextHolder m) = Counter m
type instance Distribution (ContextHolder m) = Distribution m
type instance SharedExclusiveT (ContextHolder m) = SharedExclusiveT m
type instance Gauge (ContextHolder m) = Gauge m
type instance ChannelT (ContextHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ContextHolder m) (ReaderT WalletContext m)
         , MFunctor' d (ReaderT WalletContext m) m
         ) => Mockable d (ContextHolder m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WithWalletContext (ContextHolder m) where
    getWalletContext = ContextHolder ask

instance Monad m => MonadReportingMem (ContextHolder m) where
    askReportingContext = pure $ ReportingContext []
