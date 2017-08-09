-- | Utilities used by slotting implementations.

module Pos.Slotting.Impl.Util
       ( approxSlotUsingOutdated
       , slotFromTimestamp
       ) where

import           Universum

import           Data.Time.Units             (Microsecond, convertUnit)
import           NTP.Example                 ()

import           Pos.Core                    (BlockCount, EpochIndex, HasCoreConstants,
                                              SlotId (..), Timestamp (..),
                                              addTimeDiffToTimestamp, blkSecurityParamM)
import qualified Pos.Core.Constants          as C
import           Pos.Core.Slotting           (flattenEpochIndex, unflattenSlotId)

import           Pos.Slotting.MemState.Class (MonadSlotsData (..))
import           Pos.Slotting.Types          (EpochSlottingData (..), SlottingData (..))

-- | Approximate current slot using outdated slotting data.
approxSlotUsingOutdated ::
       (MonadSlotsData m, MonadReader ctx m, HasCoreConstants ctx)
    => EpochIndex
    -> Timestamp
    -> m SlotId
approxSlotUsingOutdated penult t = do
    SlottingData {..} <- getSlottingData
    blkSecurityParam <- blkSecurityParamM
    systemStart <- getSystemStart
    let epochStart = esdStartDiff sdLast `addTimeDiffToTimestamp` systemStart
    pure $
        if | t < epochStart -> SlotId (penult + 1) 0
           | otherwise ->
               outdatedEpoch blkSecurityParam systemStart t (penult + 1) sdLast
  where
    outdatedEpoch k systemStart (Timestamp curTime) epoch EpochSlottingData {..} =
        let duration = convertUnit esdSlotDuration
            start =
                getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)
        in unflattenSlotId k $
           flattenEpochIndex k epoch +
           fromIntegral ((curTime - start) `div` duration)

-- | Compute current slot from current timestamp based on data
-- provided by 'MonadSlotsData'.
slotFromTimestamp
    :: (MonadSlotsData m, MonadReader ctx m, HasCoreConstants ctx)
    => Timestamp -> m (Maybe SlotId)
slotFromTimestamp approxCurTime = do
    SlottingData {..} <- getSlottingData
    systemStart <- getSystemStart
    k <- blkSecurityParamM
    let tryEpoch = computeSlotUsingEpoch k systemStart approxCurTime
    let penultRes = tryEpoch sdPenultEpoch sdPenult
    let lastRes = tryEpoch (succ sdPenultEpoch) sdLast
    return $ penultRes <|> lastRes

computeSlotUsingEpoch ::
       BlockCount
    -> Timestamp
    -> Timestamp
    -> EpochIndex
    -> EpochSlottingData
    -> Maybe SlotId
computeSlotUsingEpoch k systemStart (Timestamp curTime) epoch EpochSlottingData {..}
    | curTime < epochStart = Nothing
    | curTime < epochStart + epochDuration = Just $ SlotId epoch localSlot
    | otherwise = Nothing
  where
    epochStart = getTimestamp (esdStartDiff `addTimeDiffToTimestamp` systemStart)
    localSlot = fromIntegral $ (curTime - epochStart) `div` slotDuration

    slotDuration, epochDuration :: Microsecond
    slotDuration = convertUnit esdSlotDuration
    epochDuration = slotDuration * fromIntegral (C.epochSlots k)
