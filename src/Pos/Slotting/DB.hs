{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Implementation of 'MonadSlotsData' which uses 'MonadDB'.

module Pos.Slotting.DB
       ( DBSlotsData (..)
       , SlottingVar
       , runDBSlotsData
       , mkSlottingVar
       ) where

import           Control.Concurrent.STM      (TVar, newTVarIO, readTVar, writeTVar)
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

import           Pos.Context.Class           (WithNodeContext)
import           Pos.DB.Class                (MonadDB)
import qualified Pos.DB.GState               as GState
import           Pos.Slotting.Class          (MonadSlotsData (..))
import           Pos.Slotting.Types          (SlottingData (sdPenultEpoch))
import           Pos.Util.JsonLog            (MonadJL)

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

type SlottingVar = TVar SlottingData

-- | Monad transformer which provides 'SlottingData' using DB.
newtype DBSlotsData m a = DBSlotsData
    { getDBSlotsData :: ReaderT SlottingVar m a
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

               , MonadDB σ
               , WithNodeContext ssc
               , MonadJL
               )

----------------------------------------------------------------------------
-- Common instances used all over the code
----------------------------------------------------------------------------

type instance ThreadId (DBSlotsData m) = ThreadId m
type instance Promise (DBSlotsData m) = Promise m
type instance SharedAtomicT (DBSlotsData m) = SharedAtomicT m
type instance Counter (DBSlotsData m) = Counter m
type instance Distribution (DBSlotsData m) = Distribution m
type instance SharedExclusiveT (DBSlotsData m) = SharedExclusiveT m
type instance Gauge (DBSlotsData m) = Gauge m
type instance ChannelT (DBSlotsData m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (DBSlotsData m) (ReaderT SlottingVar m)
         , MFunctor' d (ReaderT SlottingVar m) m
         ) => Mockable d (DBSlotsData m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (DBSlotsData m) where
    type UnwrappedM (DBSlotsData m) = ReaderT SlottingVar m
    _WrappedM = iso getDBSlotsData DBSlotsData

instance MonadTransControl DBSlotsData where
    type StT DBSlotsData a = StT (ReaderT SlottingVar) a
    liftWith = defaultLiftWith DBSlotsData getDBSlotsData
    restoreT = defaultRestoreT DBSlotsData

instance MonadBaseControl IO m => MonadBaseControl IO (DBSlotsData m) where
    type StM (DBSlotsData m) a = ComposeSt DBSlotsData m a
    liftBaseWith = defaultLiftBaseWith
    restoreM     = defaultRestoreM

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

instance MonadIO m =>
         MonadSlotsData (DBSlotsData m) where
    getSlottingData = atomically . readTVar =<< DBSlotsData ask
    waitPenultEpochEquals target = do
        var <- DBSlotsData ask
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch /= target) retry
    putSlottingData sd = do
        var <- DBSlotsData ask
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

-- | Run USHolder using existing 'SlottingVar'.
runDBSlotsData :: SlottingVar -> DBSlotsData m a -> m a
runDBSlotsData v = usingReaderT v . getDBSlotsData

-- | Create new 'SlottingVar' using data from DB.
mkSlottingVar :: MonadDB ε m => m SlottingVar
mkSlottingVar = do
    sd <- GState.getSlottingData
    liftIO $ newTVarIO sd
