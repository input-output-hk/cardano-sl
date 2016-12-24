{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Default implementation of MonadSscLD.

module Pos.Ssc.Extra.LocalData
       ( SscLDImpl (..)
       , runSscLDImpl
       ) where

import qualified Control.Concurrent.STM      as STM
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Catch         (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Reader        (ReaderT (ReaderT), ask)
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

import           Pos.Context                 (WithNodeContext)
import qualified Pos.DB                      as Modern
import           Pos.Slotting                (MonadSlots (..))
import           Pos.Ssc.Class.LocalData     (SscLocalDataClass (sscEmptyLocalData))
import           Pos.Ssc.Class.Types         (Ssc (SscLocalData))
import           Pos.Ssc.Extra.MonadLD       (MonadSscLD (..))
import           Pos.State                   (MonadDB (..))
import           Pos.Util.JsonLog            (MonadJL (..))

newtype SscLDImpl ssc m a = SscLDImpl
    { getSscLDImpl :: ReaderT (STM.TVar (SscLocalData ssc)) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow, MonadSlots,
                MonadCatch, MonadIO, HasLoggerName, MonadDialog s p, WithNodeContext ssc, MonadJL,
                MonadDB ssc, CanLog)

instance Monad m => WrappedM (SscLDImpl ssc m) where
    type UnwrappedM (SscLDImpl ssc m) = ReaderT (STM.TVar (SscLocalData ssc)) m
    _WrappedM = iso getSscLDImpl SscLDImpl

deriving instance Modern.MonadDB ssc m => Modern.MonadDB ssc (SscLDImpl ssc m)

monadMaskHelper
    :: (ReaderT (STM.TVar (SscLocalData ssc)) m a -> ReaderT (STM.TVar (SscLocalData ssc)) m a)
    -> SscLDImpl ssc m a
    -> SscLDImpl ssc m a
monadMaskHelper u (SscLDImpl b) = SscLDImpl (u b)

instance MonadMask m =>
         MonadMask (SscLDImpl ssc m) where
    mask a = SscLDImpl $ mask $ \u -> getSscLDImpl $ a $ monadMaskHelper u
    uninterruptibleMask a =
        SscLDImpl $
        uninterruptibleMask $ \u -> getSscLDImpl $ a $ monadMaskHelper u

instance MonadIO m =>
         MonadSscLD ssc (SscLDImpl ssc m) where
    getLocalData = atomically . STM.readTVar =<< SscLDImpl ask
    setLocalData d = atomically . flip STM.writeTVar d =<< SscLDImpl ask

runSscLDImpl
    :: forall ssc m a.
       (MonadIO m, SscLocalDataClass ssc)
    => SscLDImpl ssc m a -> m a
runSscLDImpl action = do
  ref <- liftIO $ STM.newTVarIO (sscEmptyLocalData @ssc)
  flip runReaderT ref . getSscLDImpl @ssc $ action

instance MonadBase IO m => MonadBase IO (SscLDImpl ssc m) where
    liftBase = lift . liftBase

instance MonadTransControl (SscLDImpl ssc) where
    type StT (SscLDImpl ssc) a = StT (ReaderT (STM.TVar (SscLocalData ssc))) a
    liftWith = defaultLiftWith SscLDImpl getSscLDImpl
    restoreT = defaultRestoreT SscLDImpl

instance MonadBaseControl IO m => MonadBaseControl IO (SscLDImpl ssc m) where
    type StM (SscLDImpl ssc m) a = ComposeSt (SscLDImpl ssc) m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

type instance ThreadId (SscLDImpl ssc m) = ThreadId m

instance MonadTransfer s m => MonadTransfer s (SscLDImpl ssc m)
