{-# LANGUAGE TypeFamilies #-}

-- | Default implementation of 'MonadSlotsData' based on 'TVar'.

module Pos.Slotting.MemState
       ( HasSlottingVar(..)
       , MonadSlotsData
       , SlottingVar
       , cloneSlottingVar
       , withSlottingVarAtomM
       , getSystemStartM
       , getAllEpochIndicesM
       , getCurrentNextEpochIndexM
       , getCurrentNextEpochSlottingDataM
       , getEpochSlottingDataM
       , putEpochSlottingDataM
       , waitCurrentEpochEqualsM
       ) where

import           Universum

import           Control.Monad.STM (retry)

import           Pos.Core.Slotting (EpochIndex, Timestamp)
import           Pos.Slotting.Types (EpochSlottingData, SlottingData, getAllEpochIndices,
                                     getCurrentEpochIndex, getCurrentEpochSlottingData,
                                     getNextEpochIndex, getNextEpochSlottingData,
                                     insertEpochSlottingDataUnsafe, lookupEpochSlottingData)

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

type MonadSlotsData ctx m =
    ( MonadReader ctx m
    , HasSlottingVar ctx
    , MonadIO m
    )

-- TODO(ks): We might need a functions similar to `withSlottingDataAtomM` that
-- executes an STM action (function) and returns the result in @IO@ monad.
-- We might need that once we have some more complicated functions that require
-- @SlottingVar@ as a concurrency primitive.

-- A common function we extracted. It executes a pure function that requires
-- @SlottingData@ and returns the result in @IO@ monad.
-- If you need some specific portions of @SlottingData@ in your function that
-- aren't described by the primitives below, then you probably need to use this.
withSlottingVarAtomM
    :: MonadSlotsData ctx m
    => (SlottingData -> b)
    -> m b
withSlottingVarAtomM f = do
    var <- view slottingVar
    atomically $ f <$> readTVar var

getSystemStartM
    :: MonadSlotsData ctx m
    => m Timestamp
getSystemStartM =
    view slottingTimestamp

getAllEpochIndicesM
    :: MonadSlotsData ctx m
    => m [EpochIndex]
getAllEpochIndicesM =
    withSlottingVarAtomM getAllEpochIndices

-- | We get the current and the next epoch index atomically since we don't want to
-- allow a chance that a timing issue messes up the indexes - to return say (13,13).
getCurrentNextEpochIndexM
    :: MonadSlotsData ctx m
    => m (EpochIndex, EpochIndex)
getCurrentNextEpochIndexM =
    withSlottingVarAtomM getCurrentNextIndex
  where
    getCurrentNextIndex :: SlottingData -> (EpochIndex, EpochIndex)
    getCurrentNextIndex slottingData =
        (getCurrentEpochIndex slottingData, getNextEpochIndex slottingData)

-- | As with the function above, we don't want to get into a situation where we
-- can have timing issues and recieve wrong results.
getCurrentNextEpochSlottingDataM
    :: MonadSlotsData ctx m
    => m (EpochSlottingData, EpochSlottingData)
getCurrentNextEpochSlottingDataM =
    withSlottingVarAtomM getCurrentNextEpochSlottingData
  where
    getCurrentNextEpochSlottingData
        :: SlottingData
        -> (EpochSlottingData, EpochSlottingData)
    getCurrentNextEpochSlottingData sd =
        (getCurrentEpochSlottingData sd, getNextEpochSlottingData sd)

getEpochSlottingDataM
    :: MonadSlotsData ctx m
    => EpochIndex
    -> m (Maybe EpochSlottingData)
getEpochSlottingDataM ei =
    withSlottingVarAtomM $ lookupEpochSlottingData ei

-- TODO(ks): This is also quite unsafe. We should switch to @addEpochSlottingDataM@.
putEpochSlottingDataM
    :: MonadSlotsData ctx m
    => EpochIndex
    -> EpochSlottingData
    -> m ()
putEpochSlottingDataM ei esd = do
    var <- view slottingVar
    atomically $ do
        slottingData <- readTVar var
        writeTVar var (insertEpochSlottingDataUnsafe ei esd slottingData)

waitCurrentEpochEqualsM
    :: MonadSlotsData ctx m
    => EpochIndex
    -> m ()
waitCurrentEpochEqualsM target = do
    var <- view slottingVar
    atomically $ do
        currentEpoch <- getCurrentEpochIndex <$> readTVar var
        --  Wait until current epoch is >= target.
        when (currentEpoch < target) retry
