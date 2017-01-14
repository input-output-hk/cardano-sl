{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting types.

module Pos.Types.Slotting
       ( flattenSlotId
       , flattenEpochIndex
       , flattenEpochOrSlot
       , unflattenSlotId
       , crucialSlot
       ) where

import           Universum

import           Pos.Constants   (epochSlots, slotSecurityParam)
import           Pos.Types.Types (EpochIndex (..), FlatSlotId, HasEpochOrSlot,
                                  SlotId (..), _getEpochOrSlot)

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
