-- | Utilities used by slotting implementations.

module Pos.Slotting.Impl.Util
       ( approxSlotUsingOutdated
       , slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units       (Microsecond, convertUnit)
import           NTP.Example           ()

import           Pos.Core.Context      (HasCoreConstants, epochSlots)
import           Pos.Core.Slotting     (flattenEpochIndex, mkLocalSlotIndex,
                                        unflattenSlotId)
import           Pos.Core.Timestamp    (addTimeDiffToTimestamp)
import           Pos.Core.Types        (EpochIndex, SlotId (..), Timestamp (..))
import           Pos.Util.Util         (leftToPanic)

import           Pos.Slotting.MemState (MonadSlotsData, getAllEpochIndicesM,
                                        getEpochSlottingDataM, getSystemStartM,
                                        withSlottingVarAtomM)
import           Pos.Slotting.Types    (EpochSlottingData (..), SlottingData,
                                        getCurrentEpochIndex, getNextEpochSlottingData)

-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated
    :: (MonadSlotsData ctx m, HasCoreConstants)
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
    :: (MonadSlotsData ctx m, HasCoreConstants)
    => Timestamp
    -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do

    systemStart     <- getSystemStartM
    allEpochIndex   <- getAllEpochIndicesM

    -- We first reverse the indices since the most common calls to this funcion
    -- will be from next/current index and close.
    let reversedEpochIndices = reverse allEpochIndex

    let iterateIndicesUntilJust
            :: (MonadSlotsData ctx m)
            => [EpochIndex]
            -> m (Maybe SlotId)
        iterateIndicesUntilJust []      = pure Nothing
        iterateIndicesUntilJust indices = do
          -- Get current index or return @Nothing@ if the indices list is empty.
          let mCurrentIndex :: Maybe EpochIndex
              mCurrentIndex = head indices
          -- Try to find a slot.
          mFoundSlot <- maybe (pure Nothing) (findSlot systemStart) mCurrentIndex

          case mFoundSlot of
            -- If you found a slot, then return it
            Just foundSlot -> pure $ Just foundSlot
            -- If no slot is found, iterate with the rest of the list
            Nothing        -> iterateIndicesUntilJust $ tailSafe indices

    -- Iterate the indices recursively with the reverse indices, starting
    -- with the most recent one.
    iterateIndicesUntilJust reversedEpochIndices
  where

    -- Find a slot using timestamps. If no @EpochSlottingData@ is found return
    -- @Nothing@.
    findSlot
        :: (MonadSlotsData ctx m)
        => Timestamp
        -> EpochIndex
        -> m (Maybe SlotId)
    findSlot systemStart epochIndex = do
      mEpochSlottingData <- getEpochSlottingDataM epochIndex
      -- Chaining @Maybe@ since they both return it.
      pure $ do
        epochSlottingData <- mEpochSlottingData
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
        epochStart = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)
        localSlotNumeric = fromIntegral $ (curTime - epochStart) `div` slotDuration
        localSlot =
            leftToPanic "computeSlotUsingEpoch: " $
            mkLocalSlotIndex localSlotNumeric

        slotDuration :: Microsecond
        slotDuration = convertUnit esdSlotDuration

        epochDuration :: Microsecond
        epochDuration = slotDuration * fromIntegral epochSlots
