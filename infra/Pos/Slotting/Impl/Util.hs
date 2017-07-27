-- | Utilities used by slotting implementations.

module Pos.Slotting.Impl.Util
       ( approxSlotUsingOutdated
       , slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units             (Microsecond, convertUnit)
import           NTP.Example                 ()

import qualified Pos.Core.Constants          as C
import           Pos.Core.Slotting           (flattenEpochIndex, unflattenSlotId)
import           Pos.Core.Timestamp          (addTimeDiffToTimestamp)
import           Pos.Core.Types              (EpochIndex, SlotId (..), Timestamp (..),
                                              mkLocalSlotIndex)
import           Pos.Util.Util               (leftToPanic)

import           Pos.Slotting.Util           (getEpochSlottingDataEmpatically)
import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..))

-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated
    :: (MonadSlotsData m)
    => EpochIndex
    -> Timestamp
    -> m SlotId
approxSlotUsingOutdated penult t = do
    systemStart <- getSystemStartM
    sdLast      <- getNextEpochSlottingDataM
    let epochStart = esdStartDiff sdLast `addTimeDiffToTimestamp` systemStart
    pure $
        if | t < epochStart -> SlotId (penult + 1) minBound
           | otherwise      -> outdatedEpoch systemStart t (penult + 1) sdLast
  where
    outdatedEpoch systemStart (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)
        in
        unflattenSlotId $
        flattenEpochIndex epoch + fromIntegral ((curTime - start) `div` duration)

-- TODO(ks): Move to MonadSlotsData?
-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: (MonadSlotsData m, MonadThrow m)
    => Timestamp
    -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do

    systemStart     <- getSystemStartM
    allEpochIndex   <- getAllEpochIndexM

    -- We partially apply the function and then iterate over all epoch indexes.
    mSlotId         <- forM allEpochIndex (findSlot systemStart approxCurTime)

    pure $ head $ catMaybes mSlotId

  where

    findSlot
        :: (MonadSlotsData m, MonadThrow m)
        => Timestamp
        -> Timestamp
        -> EpochIndex
        -> m (Maybe SlotId)
    findSlot systemStart approxCurTime' epochIndex = do
        epochSlotData <- getEpochSlottingDataEmpatically epochIndex
        pure $ computeSlotUsingEpoch systemStart approxCurTime' epochIndex epochSlotData

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
        epochDuration = slotDuration * fromIntegral C.epochSlots
