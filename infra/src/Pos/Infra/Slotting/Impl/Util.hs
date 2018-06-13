{-# LANGUAGE RecordWildCards #-}

-- | Utilities used by slotting implementations.

module Pos.Infra.Slotting.Impl.Util
       ( approxSlotUsingOutdated

       -- Re-exported from Core
       , slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units (convertUnit)

import           Pos.Core.Slotting (EpochIndex, EpochSlottingData (..),
                     MonadSlotsData, SlotCount, SlotId (..), SlottingData,
                     Timestamp (..), addTimeDiffToTimestamp, flattenEpochIndex,
                     getSystemStartM, localSlotIndexMinBound,
                     slotFromTimestamp, unflattenSlotId, withSlottingVarAtomM)
import           Pos.Infra.Slotting.Types (getCurrentEpochIndex,
                     getNextEpochSlottingData)


-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated
    :: MonadSlotsData ctx m
    => SlotCount
    -> Timestamp
    -> m SlotId
approxSlotUsingOutdated epochSlots t = do

    -- This is a constant and doesn't need to be fetched atomically
    systemStart <- getSystemStartM

    -- This is here to ensure we don't have any timing issues
    (currentEpochIndex, nextSlottingData) <- withSlottingVarAtomM slottingDataAtomically

    let epochStart = esdStartDiff nextSlottingData `addTimeDiffToTimestamp` systemStart
    pure $
        if | t < epochStart -> SlotId (currentEpochIndex + 1) localSlotIndexMinBound
           | otherwise      -> outdatedEpoch systemStart t (currentEpochIndex + 1) nextSlottingData
  where
    outdatedEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)
        in
        unflattenSlotId epochSlots $
        flattenEpochIndex epochSlots epoch + fromIntegral ((curTime - start) `div` duration)

    -- | Get both values we need in a single fetch, so we don't end up with
    -- invalid data.
    slottingDataAtomically
        :: SlottingData
        -> (EpochIndex, EpochSlottingData)
    slottingDataAtomically slottingData =
        ( getCurrentEpochIndex slottingData
        , getNextEpochSlottingData slottingData
        )
