{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( HasSlottingVar(..)
       , SlottingVar
       , cloneSlottingVar
       , getSystemStartDefault
       , getEpochLastIndexDefault
       , getEpochSlottingDataDefault
       , putEpochSlottingDataDefault
       , waitPenultEpochEqualsDefault
       ) where

import           Universum

import           Control.Monad.STM  (retry)

import           Pos.Core.Types     (EpochIndex, Timestamp)
import           Pos.Slotting.Types (EpochSlottingData, SlottingData,
                                     addEpochSlottingData,
                                     getLastEpochIndex,
                                     getPenultEpochIndex,
                                     lookupEpochSlottingData)

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
    slottingVar       :: Lens' ctx SlottingVar

----------------------------------------------------------------------------
-- MonadSlotsData implementation
----------------------------------------------------------------------------

type SlotsDefaultEnv ctx m =
    (MonadReader ctx m, HasSlottingVar ctx, MonadIO m)

getSystemStartDefault 
    :: SlotsDefaultEnv ctx m 
    => m Timestamp
getSystemStartDefault = view slottingTimestamp

getEpochLastIndexDefault 
    :: SlotsDefaultEnv ctx m 
    => m EpochIndex
getEpochLastIndexDefault = do
    var <- view slottingVar
    atomically $ getLastEpochIndex <$> readTVar var

getEpochSlottingDataDefault 
    :: SlotsDefaultEnv ctx m 
    => EpochIndex 
    -> m (Maybe EpochSlottingData)
getEpochSlottingDataDefault ei = do
    var <- view slottingVar
    atomically $ lookupEpochSlottingData ei <$> readTVar var

putEpochSlottingDataDefault 
    :: SlotsDefaultEnv ctx m 
    => EpochIndex 
    -> EpochSlottingData 
    -> m ()
putEpochSlottingDataDefault ei esd = do
    var <- view slottingVar
    atomically $ do
        slottingData <- readTVar var
        writeTVar var (addEpochSlottingData ei esd slottingData)

waitPenultEpochEqualsDefault 
    :: SlotsDefaultEnv ctx m 
    => EpochIndex 
    -> m ()
waitPenultEpochEqualsDefault target = do
    var <- view slottingVar
    atomically $ do
        penultEpoch <- getPenultEpochIndex <$> readTVar var
        when (penultEpoch /= target) retry
