{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState.Holder
       ( HasSlottingVar(..)
       , SlottingVar
       , cloneSlottingVar
       , getSystemStartDefault
       , getAllEpochIndicesDefault
       , getCurrentNextEpochIndexDefault
       , getCurrentNextEpochSlottingDataDefault
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

-- A common function we extracted. It executes a pure function that requires
-- @SlottingData@ and returns the result in @STM@ monad.
withSlottingVar
    :: SlotsDefaultEnv ctx m
    => (SlottingData -> b)
    -> m b
withSlottingVar f = do
    var <- view slottingVar
    atomically $ f <$> readTVar var

getSystemStartDefault
    :: SlotsDefaultEnv ctx m
    => m Timestamp
getSystemStartDefault =
    view slottingTimestamp

getAllEpochIndicesDefault
    :: SlotsDefaultEnv ctx m
    => m [EpochIndex]
getAllEpochIndicesDefault =
    withSlottingVar getAllEpochIndices

-- | We get the current and the next epoch index atomically since we don't want to
-- allow a chance that a timing issue messes up the indexes - to return say (13,13).
getCurrentNextEpochIndexDefault
    :: SlotsDefaultEnv ctx m
    => m (EpochIndex, EpochIndex)
getCurrentNextEpochIndexDefault =
    withSlottingVar getCurrentNextIndex
  where
    getCurrentNextIndex :: SlottingData -> (EpochIndex, EpochIndex)
    getCurrentNextIndex slottingData =
        (getCurrentEpochIndex slottingData, getNextEpochIndex slottingData)

-- | As with the function above, we don't want to get into a situation where we
-- can have timing issues and recieve wrong results.
getCurrentNextEpochSlottingDataDefault
    :: SlotsDefaultEnv ctx m
    => m (EpochSlottingData, EpochSlottingData)
getCurrentNextEpochSlottingDataDefault =
    withSlottingVar getCurrentNextEpochSlottingData
  where
    getCurrentNextEpochSlottingData
        :: SlottingData
        -> (EpochSlottingData, EpochSlottingData)
    getCurrentNextEpochSlottingData sd =
        (getCurrentEpochSlottingData sd, getNextEpochSlottingData sd)

getEpochSlottingDataDefault
    :: SlotsDefaultEnv ctx m
    => EpochIndex
    -> m (Maybe EpochSlottingData)
getEpochSlottingDataDefault ei =
    withSlottingVar $ lookupEpochSlottingData ei

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

