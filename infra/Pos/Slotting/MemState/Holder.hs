{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( HasSlottingVar(..)
       , SlottingVar
       , cloneSlottingVar
       , getSystemStartDefault
       , getSlottingDataDefault
       , waitPenultEpochEqualsDefault
       , putSlottingDataDefault
       ) where

import           Universum

import           Control.Monad.STM  (retry)

import           Pos.Core.Types     (EpochIndex, Timestamp)
import           Pos.Slotting.Types (SlottingData (sdPenultEpoch))

----------------------------------------------------------------------------
-- Context
----------------------------------------------------------------------------

type SlottingVar = TVar SlottingData

-- | Create a new 'SlottingVar' with the same contents as the given
-- variable has.
cloneSlottingVar :: MonadIO m => SlottingVar -> m SlottingVar
cloneSlottingVar = readTVarIO >=> newTVarIO

-- | System start and slotting data
class HasSlottingVar ctx where
    slottingTimestamp :: Lens' ctx Timestamp
    slottingVar :: Lens' ctx SlottingVar

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

type SlotsDefaultEnv ctx m =
    (MonadReader ctx m, HasSlottingVar ctx, MonadIO m)

getSystemStartDefault :: SlotsDefaultEnv ctx m => m Timestamp
getSystemStartDefault = view slottingTimestamp

getSlottingDataDefault :: SlotsDefaultEnv ctx m => m SlottingData
getSlottingDataDefault = atomically . readTVar =<< view slottingVar

waitPenultEpochEqualsDefault :: SlotsDefaultEnv ctx m => EpochIndex -> m ()
waitPenultEpochEqualsDefault target = do
    var <- view slottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch /= target) retry

putSlottingDataDefault :: SlotsDefaultEnv ctx m => SlottingData -> m ()
putSlottingDataDefault sd = do
    var <- view slottingVar
    atomically $ do
        penultEpoch <- sdPenultEpoch <$> readTVar var
        when (penultEpoch < sdPenultEpoch sd) $ writeTVar var sd
