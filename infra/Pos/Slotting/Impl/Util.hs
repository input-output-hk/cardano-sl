-- | Utilities used by slotting implementations.

module Pos.Slotting.Impl.Util
       ( approxSlotUsingOutdated
       , slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units (Microsecond, convertUnit)

import           Pos.Core.Configuration (epochSlots, HasProtocolConstants)
import           Pos.Core.Slotting (EpochIndex, LocalSlotIndex, SlotId (..), Timestamp (..),
                                    addTimeDiffToTimestamp, flattenEpochIndex, mkLocalSlotIndex,
                                    unflattenSlotId)
import           Pos.Util.Util (leftToPanic)

import           Pos.Slotting.MemState (MonadSlotsData, getSystemStartM, withSlottingVarAtomM)
import           Pos.Slotting.Types (EpochSlottingData (..), SlottingData, getAllEpochIndices,
                                     getCurrentEpochIndex, getNextEpochSlottingData,
                                     lookupEpochSlottingData)

-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated
    :: (MonadSlotsData ctx m, HasProtocolConstants)
    => Timestamp
    -> m SlotId
approxSlotUsingOutdated t = do

    -- This is a constant and doesn't need to be fetched atomically
    systemStart <- getSystemStartM

    -- This is here to ensure we don't have any timing issues
    (currentEpochIndex, nextSlottingData) <- withSlottingVarAtomM slottingDataAtomically

    let epochStart = esdStartDiff nextSlottingData `addTimeDiffToTimestamp` systemStart
    pure $
        if | t < epochStart -> SlotId (currentEpochIndex + 1) minBound
           | otherwise      -> outdatedEpoch systemStart t (currentEpochIndex + 1) nextSlottingData
  where
    outdatedEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)
        in
        unflattenSlotId $
        flattenEpochIndex epoch + fromIntegral ((curTime - start) `div` duration)

    -- | Get both values we need in a single fetch, so we don't end up with
    -- invalid data.
    slottingDataAtomically
        :: SlottingData
        -> (EpochIndex, EpochSlottingData)
    slottingDataAtomically slottingData =
        ( getCurrentEpochIndex slottingData
        , getNextEpochSlottingData slottingData
        )

-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: (MonadSlotsData ctx m, HasProtocolConstants)
    => Timestamp
    -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do
    systemStart <- getSystemStartM
    withSlottingVarAtomM (iterateBackwardsSearch systemStart)
  where
    iterateBackwardsSearch
        :: Timestamp
        -> SlottingData
        -> Maybe SlotId
    iterateBackwardsSearch systemStart slottingData = do
        let allEpochIndex = getAllEpochIndices slottingData

        -- We first reverse the indices since the most common calls to this funcion
        -- will be from next/current index and close.
        let reversedEpochIndices = reverse allEpochIndex

        let iterateIndicesUntilJust
                :: [EpochIndex]
                -> Maybe SlotId
            iterateIndicesUntilJust []      = Nothing
            iterateIndicesUntilJust indices = do

              -- Get current index or return @Nothing@ if the indices list is empty.
              let mCurrentEpochIndex :: Maybe EpochIndex
                  mCurrentEpochIndex = case indices of
                      (x:_) -> Just x
                      _     -> Nothing
              -- Try to find a slot.
              let mFoundSlot = mCurrentEpochIndex >>= findSlot systemStart slottingData

              case mFoundSlot of
                -- If you found a slot, then return it
                Just foundSlot -> Just foundSlot
                -- If no slot is found, iterate with the rest of the list
                Nothing        -> iterateIndicesUntilJust $ case indices of
                    []     -> []
                    (_:xs) -> xs


        -- Iterate the indices recursively with the reverse indices, starting
        -- with the most recent one.
        iterateIndicesUntilJust reversedEpochIndices


    -- Find a slot using timestamps. If no @EpochSlottingData@ is found return
    -- @Nothing@.
    findSlot
        :: Timestamp
        -> SlottingData
        -> EpochIndex
        -> Maybe SlotId
    findSlot systemStart slottingData epochIndex = do
        epochSlottingData <- lookupEpochSlottingData epochIndex slottingData
        computeSlotUsingEpoch systemStart approxCurTime epochIndex epochSlottingData

    computeSlotUsingEpoch
        :: Timestamp
        -> Timestamp
        -> EpochIndex
        -> EpochSlottingData
        -> Maybe SlotId
    computeSlotUsingEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..}
        | curTime < epochStart = Nothing
        | curTime < epochStart + epochDuration = Just $ SlotId epoch localSlot
        | otherwise = Nothing
      where
        epochStart :: Microsecond
        epochStart = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)

        localSlotNumeric :: Word16
        localSlotNumeric = fromIntegral $ (curTime - epochStart) `div` slotDuration

        localSlot :: LocalSlotIndex
        localSlot =
            leftToPanic "computeSlotUsingEpoch: " $
            mkLocalSlotIndex localSlotNumeric

        slotDuration :: Microsecond
        slotDuration = convertUnit esdSlotDuration

        epochDuration :: Microsecond
        epochDuration = slotDuration * fromIntegral epochSlots
