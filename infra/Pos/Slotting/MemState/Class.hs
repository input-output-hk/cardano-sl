{-# LANGUAGE TypeFamilies #-}

module Pos.Slotting.MemState.Class
       ( MonadSlotsData (..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Universum

import           Pos.Core.Types      (EpochIndex, Timestamp)
import           Pos.Slotting.Types  (SlottingData)

-- | 'MonadSlotsData' provides access to data necessary for slotting to work.
class Monad m => MonadSlotsData m where

    getSystemStart :: m Timestamp

    getSlottingData :: m SlottingData

    waitPenultEpochEquals :: EpochIndex -> m ()

    putSlottingData :: SlottingData -> m ()


instance {-# OVERLAPPABLE #-}
    (MonadSlotsData m, MonadTrans t, Monad (t m)) =>
        MonadSlotsData (t m) where
    getSystemStart = lift getSystemStart
    getSlottingData = lift getSlottingData
    waitPenultEpochEquals = lift . waitPenultEpochEquals
    putSlottingData = lift . putSlottingData
