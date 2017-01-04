{-# LANGUAGE ScopedTypeVariables #-}

-- | Slotting types.

module Pos.Types.Slotting
       ( flattenSlotId
       , flattenEpochIndex
       , flattenEpochOrSlot
       , unflattenSlotId
       , subSlotSafe
       ) where

import           Universum

import           Pos.Constants   (epochSlots)
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

subSlotSafe :: SlotId -> Word64 -> SlotId
subSlotSafe (flattenSlotId -> slotId) diff
    | diff >= slotId = unflattenSlotId 0
    | otherwise      = unflattenSlotId (slotId - diff)
