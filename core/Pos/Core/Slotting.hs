-- | Slotting functions.

module Pos.Core.Slotting
       ( flattenSlotId
       , flattenEpochIndex
       , flattenEpochOrSlot
       , unflattenSlotId
       , crucialSlot
       ) where

import           Universum

import           Pos.Core.Constants (epochSlots, slotSecurityParam)
import           Pos.Core.Types     (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                     HasEpochOrSlot (..), SlotId (..))

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: SlotId -> FlatSlotId
flattenSlotId SlotId {..} = fromIntegral siEpoch * epochSlots + fromIntegral siSlot

-- | Flattens 'EpochIndex' into a single number.
flattenEpochIndex :: EpochIndex -> FlatSlotId
flattenEpochIndex EpochIndex {..} = getEpochIndex * epochSlots

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: (HasEpochOrSlot a) => a -> FlatSlotId
flattenEpochOrSlot =
    either flattenEpochIndex flattenSlotId . _getEpochOrSlot

-- | Construct 'SlotId' from a flattened variant.
unflattenSlotId :: FlatSlotId -> SlotId
unflattenSlotId n =
    let (fromIntegral -> siEpoch, fromIntegral -> siSlot) =
            n `divMod` epochSlots
    in SlotId {..}

instance Enum SlotId where
    toEnum = unflattenSlotId . fromIntegral
    fromEnum = fromIntegral . flattenSlotId

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: EpochIndex -> SlotId
crucialSlot 0        = SlotId {siEpoch = 0, siSlot = 0}
crucialSlot epochIdx =
    SlotId {siEpoch = epochIdx - 1, siSlot = epochSlots - slotSecurityParam - 1}

instance Enum EpochOrSlot where
    succ (EpochOrSlot (Left e)) =
        EpochOrSlot (Right SlotId {siEpoch = e, siSlot = 0})
    succ (EpochOrSlot (Right si@SlotId {..}))
        | siSlot == epochSlots - 1 && siEpoch == maxBound =
            panic "succ@EpochOrSlot: maxBound"
        | siSlot == epochSlots - 1 = EpochOrSlot (Left (siEpoch + 1))
        | otherwise = EpochOrSlot $ Right si {siSlot = siSlot + 1}
    pred (EpochOrSlot (Left e))
        | e == 0 = panic "pred@EpochOrSlot: minBound"
        | otherwise =
            EpochOrSlot
                (Right SlotId {siEpoch = e - 1, siSlot = epochSlots - 1})
    pred (EpochOrSlot (Right si@SlotId {..}))
        | siSlot == 0 = EpochOrSlot (Left siEpoch)
        | otherwise = EpochOrSlot $ Right si {siSlot = siSlot - 1}
    fromEnum (EpochOrSlot (Left e)) = fromIntegral e * epochSlots + 1
    fromEnum (EpochOrSlot (Right SlotId {..})) =
        fromEnum (EpochOrSlot (Left siEpoch)) + fromIntegral siSlot + 1
    toEnum x =
        let (fromIntegral -> epoch, fromIntegral -> slot) =
                x `divMod` (epochSlots + 1)
        in if | slot == 0 -> EpochOrSlot (Left epoch)
              | otherwise ->
                  EpochOrSlot
                      (Right SlotId {siSlot = slot - 1, siEpoch = epoch})
