{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( HasSlottingVar(..)
       , SlottingVar
       , cloneSlottingVar
       , getSystemStartDefault
       , getAllEpochIndicesDefault
       , getCurrentEpochIndexDefault
       , getCurrentEpochSlottingDataDefault
       , getNextEpochIndexDefault
       , getNextEpochSlottingDataDefault
       , getEpochSlottingDataDefault
       , putEpochSlottingDataDefault
       , waitCurrentEpochEqualsDefault
       ) where

import           Universum

import           Control.Monad.STM  (retry)

import           Pos.Core.Types     (EpochIndex, Timestamp)
import           Pos.Slotting.Types (EpochSlottingData, SlottingData,
                                     addEpochSlottingData, getAllEpochIndices,
                                     getCurrentEpochIndex, getCurrentEpochSlottingData,
                                     getNextEpochIndex, getNextEpochSlottingData,
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

getAllEpochIndicesDefault
    :: SlotsDefaultEnv ctx m
    => m [EpochIndex]
getAllEpochIndicesDefault = do
    var <- view slottingVar
    atomically $ getAllEpochIndices <$> readTVar var

getCurrentEpochIndexDefault
    :: SlotsDefaultEnv ctx m
    => m EpochIndex
getCurrentEpochIndexDefault = do
    var <- view slottingVar
    atomically $ getCurrentEpochIndex <$> readTVar var

getNextEpochIndexDefault
    :: SlotsDefaultEnv ctx m
    => m EpochIndex
getNextEpochIndexDefault = do
    var <- view slottingVar
    atomically $ getNextEpochIndex <$> readTVar var

getCurrentEpochSlottingDataDefault
    :: SlotsDefaultEnv ctx m
    => m EpochSlottingData
getCurrentEpochSlottingDataDefault = do
    var <- view slottingVar
    atomically $ getCurrentEpochSlottingData <$> readTVar var

getNextEpochSlottingDataDefault
    :: SlotsDefaultEnv ctx m
    => m EpochSlottingData
getNextEpochSlottingDataDefault = do
    var <- view slottingVar
    atomically $ getNextEpochSlottingData <$> readTVar var

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

waitCurrentEpochEqualsDefault
    :: SlotsDefaultEnv ctx m
    => EpochIndex
    -> m ()
waitCurrentEpochEqualsDefault target = do
    var <- view slottingVar
    atomically $ do
        currentEpoch <- getCurrentEpochIndex <$> readTVar var
        when (currentEpoch /= target) retry

