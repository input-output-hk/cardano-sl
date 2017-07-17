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
import           Pos.Core.Types              (EpochIndex, SlotId (..), Timestamp (..),
                                              mkLocalSlotIndex)
import           Pos.Util.Util               (leftToPanic)

import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..), SlottingData (..))

-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated :: MonadSlotsData m => EpochIndex -> Timestamp -> m SlotId
approxSlotUsingOutdated penult t = do
    SlottingData {..} <- getSlottingData
    pure $
        if | t < esdStart sdLast -> SlotId (penult + 1) minBound
           | otherwise           -> outdatedEpoch t (penult + 1) sdLast
  where
    outdatedEpoch (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start = getTimestamp esdStart in
        unflattenSlotId $
        flattenEpochIndex epoch + fromIntegral ((curTime - start) `div` duration)

-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: MonadSlotsData m
    => Timestamp -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do
    SlottingData {..} <- getSlottingData
    let tryEpoch = computeSlotUsingEpoch approxCurTime
    let penultRes = tryEpoch sdPenultEpoch sdPenult
    let lastRes = tryEpoch (succ sdPenultEpoch) sdLast
    return $ penultRes <|> lastRes

computeSlotUsingEpoch
    :: Timestamp
    -> EpochIndex
    -> EpochSlottingData
    -> Maybe SlotId
computeSlotUsingEpoch (Timestamp curTime) epoch EpochSlottingData {..}
    | curTime < start = Nothing
    | curTime < start + epochDuration = Just $ SlotId epoch localSlot
    | otherwise = Nothing
  where
    localSlotNumeric = fromIntegral $ (curTime - start) `div` slotDuration
    localSlot =
        leftToPanic "computeSlotUsingEpoch: " $
        mkLocalSlotIndex localSlotNumeric
    start = getTimestamp esdStart

    slotDuration, epochDuration :: Microsecond
    slotDuration = convertUnit esdSlotDuration
    epochDuration = slotDuration * fromIntegral C.epochSlots
