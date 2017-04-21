{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( SlottingHolder
       , SlottingVar
       , runSlottingHolder
       , MonadSlotting(..)
       , askSlottingVar
       , askSlottingTimestamp
       ) where

import           Universum

import qualified Control.Monad.Ether.Implicit as Ether
import           Control.Monad.STM           (retry)
import           Control.Monad.Trans.Class   (MonadTrans)
import           Pos.Core.Types              (Timestamp)

import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (SlottingData (sdPenultEpoch))

----------------------------------------------------------------------------
-- Transformer
----------------------------------------------------------------------------

-- | System start and slotting data
type SlottingVar = (Timestamp, TVar SlottingData)

-- | Monad transformer which provides 'SlottingData' using DB.
type SlottingHolder = Ether.ReaderT SlottingVar

class Monad m => MonadSlotting m where
  askSlotting :: m SlottingVar

  default askSlotting
    :: (MonadSlotting m', MonadTrans t, m ~ t m') => m SlottingVar
  askSlotting = lift askSlotting

instance {-# OVERLAPPABLE #-}
  (MonadSlotting m, MonadTrans t, Monad (t m)) =>
  MonadSlotting (t m)

instance Monad m => MonadSlotting (SlottingHolder m) where
  askSlotting = Ether.ask

askSlottingVar :: MonadSlotting m => m (TVar SlottingData)
askSlottingVar = snd <$> askSlotting

askSlottingTimestamp :: MonadSlotting m => m Timestamp
askSlottingTimestamp  = fst <$> askSlotting

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

instance MonadIO m =>
         MonadSlotsData (SlottingHolder m) where
    getSystemStart = askSlottingTimestamp
    getSlottingData = atomically . readTVar =<< askSlottingVar
    waitPenultEpochEquals target = do
        var <- askSlottingVar
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch /= target) retry
    putSlottingData sd = do
        var <- askSlottingVar
        atomically $ do
            penultEpoch <- sdPenultEpoch <$> readTVar var
            when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd

----------------------------------------------------------------------------
-- Running
----------------------------------------------------------------------------

-- | Run USHolder using existing 'SlottingVar'.
runSlottingHolder :: SlottingVar -> SlottingHolder m a -> m a
runSlottingHolder = flip Ether.runReaderT
