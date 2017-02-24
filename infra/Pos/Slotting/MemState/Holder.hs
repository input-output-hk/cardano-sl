{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( SlottingHolder (..)
       , SlottingVar
       , runSlottingHolder
       --, mkSlottingVar
       ) where

import           Control.Concurrent.STM      (TVar, readTVar, writeTVar)
import           Control.Lens                (iso)
import           Control.Monad.Base          (MonadBase (..))
import           Control.Monad.Fix           (MonadFix)
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

import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (SlottingData (sdPenultEpoch))

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

type SlottingVar = TVar SlottingData

-- | Monad transformer which provides 'SlottingData' using DB.
newtype SlottingHolder m a = SlottingHolder
    { getSlottingHolder :: ReaderT SlottingVar m a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadTrans
               , MonadFix

               , MonadThrow
               , MonadCatch
               , MonadMask

               , MonadBase base

               , HasLoggerName
               , CanLog
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (SlottingHolder m) = ThreadId m
type instance Promise (SlottingHolder m) = Promise m
type instance SharedAtomicT (SlottingHolder m) = SharedAtomicT m
type instance Counter (SlottingHolder m) = Counter m
type instance Distribution (SlottingHolder m) = Distribution m
type instance SharedExclusiveT (SlottingHolder m) = SharedExclusiveT m
type instance Gauge (SlottingHolder m) = Gauge m
type instance ChannelT (SlottingHolder m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (SlottingHolder m) (ReaderT SlottingVar m)
         , MFunctor' d (ReaderT SlottingVar m) m
         ) => Mockable d (SlottingHolder m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (SlottingHolder m) where
    type UnwrappedM (SlottingHolder m) = ReaderT SlottingVar m
    _WrappedM = iso getSlottingHolder SlottingHolder

instance MonadTransControl SlottingHolder where
    type StT SlottingHolder a = StT (ReaderT SlottingVar) a
    liftWith = defaultLiftWith SlottingHolder getSlottingHolder
    restoreT = defaultRestoreT SlottingHolder

instance MonadBaseControl IO m => MonadBaseControl IO (SlottingHolder m) where
    type StM (SlottingHolder m) a = ComposeSt SlottingHolder m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

instance MonadIO m =>
         MonadSlotsData (SlottingHolder m) where
    getSystemStart = notImplemented
    getSlottingData = atomically . readTVar =<< SlottingHolder ask
    waitPenultEpochEquals target = do
        var <- SlottingHolder ask
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch /= target) retry
    putSlottingData sd = do
        var <- SlottingHolder ask
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

-- | Run USHolder using existing 'SlottingVar'.
runSlottingHolder :: SlottingVar -> SlottingHolder m a -> m a
runSlottingHolder v = usingReaderT v . getSlottingHolder

-- | Create new 'SlottingVar' using data from DB.
-- mkSlottingVar :: MonadDB ε m => m SlottingVar
-- mkSlottingVar = do
--     sd <- GState.getSlottingData
--     liftIO $ newTVarIO sd
