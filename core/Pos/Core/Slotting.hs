-- | Slotting functions.

module Pos.Core.Slotting
       ( flattenSlotId
       , flattenEpochIndex
       , flattenEpochOrSlot
       , unflattenSlotId
       , diffEpochOrSlot
       , crucialSlot
       , unsafeMkLocalSlotIndex
       ) where

import           Universum

import           Pos.Core.Class     (HasEpochOrSlot (..), getEpochOrSlot)
import           Pos.Core.Constants (epochSlots, slotSecurityParam)
import           Pos.Core.Types     (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                     LocalSlotIndex, SlotId (..), epochOrSlot,
                                     getSlotIndex, mkLocalSlotIndex)
import           Pos.Util.Util      (leftToPanic)

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: SlotId -> FlatSlotId
flattenSlotId SlotId {..} =
    fromIntegral siEpoch * epochSlots + fromIntegral (getSlotIndex siSlot)

-- | Flattens 'EpochIndex' into a single number.
flattenEpochIndex :: EpochIndex -> FlatSlotId
flattenEpochIndex EpochIndex {..} = getEpochIndex * epochSlots

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: (HasEpochOrSlot a) => a -> FlatSlotId
flattenEpochOrSlot =
    epochOrSlot flattenEpochIndex flattenSlotId . getEpochOrSlot

-- | Construct 'SlotId' from a flattened variant.
unflattenSlotId :: FlatSlotId -> SlotId
unflattenSlotId n =
    let (fromIntegral -> siEpoch, fromIntegral -> slot) = n `divMod` epochSlots
        siSlot = leftToPanic "unflattenSlotId: " $ mkLocalSlotIndex slot
    in SlotId {..}

-- | Distance (in slots) between two slots. The first slot is newer, the
-- second slot is older. An epoch is considered the same as the 0th slot of
-- that epoch.
diffEpochOrSlot :: EpochOrSlot -> EpochOrSlot -> Int64
diffEpochOrSlot a b =
    fromInteger $
    toInteger (flattenEpochOrSlot a) - toInteger (flattenEpochOrSlot b)

instance Enum SlotId where
    toEnum = unflattenSlotId . fromIntegral
    fromEnum = fromIntegral . flattenSlotId

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: EpochIndex -> SlotId
crucialSlot 0        = SlotId {siEpoch = 0, siSlot = minBound}
crucialSlot epochIdx = SlotId {siEpoch = epochIdx - 1, ..}
  where
    siSlot =
        leftToPanic "crucialSlot: " $
        mkLocalSlotIndex (epochSlots - slotSecurityParam - 1)

instance Enum EpochOrSlot where
    succ (EpochOrSlot (Left e)) =
        EpochOrSlot (Right SlotId {siEpoch = e, siSlot = minBound})
    succ (EpochOrSlot (Right si@SlotId {..}))
        | siSlot == maxBound && siEpoch == maxBound =
            error "succ@EpochOrSlot: maxBound"
        | siSlot == maxBound = EpochOrSlot (Left (siEpoch + 1))
        | otherwise = EpochOrSlot $ Right si {siSlot = succ siSlot}
    pred (EpochOrSlot (Left e))
        | e == 0 = error "pred@EpochOrSlot: minBound"
        | otherwise =
            EpochOrSlot (Right SlotId {siEpoch = e - 1, siSlot = maxBound})
    pred (EpochOrSlot (Right si@SlotId {..}))
        | siSlot == minBound = EpochOrSlot (Left siEpoch)
        | otherwise = EpochOrSlot $ Right si {siSlot = pred siSlot}
    fromEnum (EpochOrSlot (Left e)) = fromIntegral e * epochSlots + 1
    fromEnum (EpochOrSlot (Right SlotId {..})) =
        fromEnum (EpochOrSlot (Left siEpoch)) +
        fromIntegral (getSlotIndex siSlot) +
        1
    toEnum x =
        let (fromIntegral -> epoch, fromIntegral -> slot) =
                x `divMod` (epochSlots + 1)
            slotIdx =
                leftToPanic "toEnum @EpochOrSlot" $ mkLocalSlotIndex (slot - 1)
        in if | slot == 0 -> EpochOrSlot (Left epoch)
              | otherwise ->
                  EpochOrSlot
                      (Right SlotId {siSlot = slotIdx, siEpoch = epoch})

-- | Unsafe constructor of 'LocalSlotIndex'.
unsafeMkLocalSlotIndex :: Word16 -> LocalSlotIndex
unsafeMkLocalSlotIndex =
    leftToPanic "unsafeMkLocalSlotIndex failed: " . mkLocalSlotIndex
