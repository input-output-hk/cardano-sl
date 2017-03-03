{-# LANGUAGE TypeFamilies #-}

module Pos.Slotting.MemState.Class
       ( MonadSlotsData (..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Universum

import           Pos.Core.Types      (EpochIndex, Timestamp)
import           Pos.Slotting.Types  (SlottingData)

-- | 'MonadSlotsData' provides access to data necessary for slotting to work.
class Monad m =>
      MonadSlotsData m where
    getSystemStart :: m Timestamp

    getSlottingData :: m SlottingData

    waitPenultEpochEquals :: EpochIndex -> m ()

    putSlottingData :: SlottingData -> m ()

    default getSystemStart :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
       m Timestamp
    getSystemStart = lift getSystemStart

    default getSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        m SlottingData
    getSlottingData = lift getSlottingData

    default waitPenultEpochEquals :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        EpochIndex -> m ()
    waitPenultEpochEquals = lift . waitPenultEpochEquals

    default putSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        SlottingData -> m ()
    putSlottingData = lift . putSlottingData

instance MonadSlotsData m => MonadSlotsData (ReaderT s m) where
instance MonadSlotsData m => MonadSlotsData (ExceptT s m) where
instance MonadSlotsData m => MonadSlotsData (StateT s m) where
