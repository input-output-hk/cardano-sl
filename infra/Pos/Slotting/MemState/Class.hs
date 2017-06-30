{-# LANGUAGE TypeFamilies #-}

module Pos.Slotting.MemState.Class
       ( MonadSlotsData (..)
       ) where

import           Control.Monad.Trans (MonadTrans)
import           Universum

import           Pos.Core.Types      (EpochIndex, Timestamp)
import           Pos.Slotting.Types  (EpochSlottingData)

-- | 'MonadSlotsData' provides access to data necessary for slotting to work.
class Monad m => MonadSlotsData m where

    getSystemStart :: m Timestamp

    getEpochLastIndex :: m EpochIndex

    getEpochSlottingData :: EpochIndex -> m (Maybe EpochSlottingData)

    waitPenultEpochEquals :: EpochIndex -> m ()

    putEpochSlottingData :: EpochIndex -> EpochSlottingData -> m ()

    default getSystemStart :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
       m Timestamp
    getSystemStart = lift getSystemStart

    default getEpochLastIndex :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
       m EpochIndex
    getEpochLastIndex = lift getEpochLastIndex

    default getEpochSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) => EpochIndex ->
        m (Maybe EpochSlottingData)
    getEpochSlottingData = lift . getEpochSlottingData

    default waitPenultEpochEquals :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        EpochIndex -> m ()
    waitPenultEpochEquals = lift . waitPenultEpochEquals

    default putEpochSlottingData :: (MonadTrans t, MonadSlotsData m', t m' ~ m) =>
        EpochIndex -> EpochSlottingData -> m ()
    putEpochSlottingData = (lift .) . putEpochSlottingData

instance {-# OVERLAPPABLE #-}
    (MonadSlotsData m, MonadTrans t, Monad (t m)) =>
        MonadSlotsData (t m)
