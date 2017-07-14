-- | Slotting functions.

module Pos.Core.Slotting
       ( flattenSlotId
       , flattenEpochIndex
       , flattenEpochOrSlot
       , unflattenSlotId
       , diffEpochOrSlot
       , crucialSlot
       , unsafeMkLocalSlotIndex
       , isBootstrapEra
       , epochOrSlot
       , epochOrSlotToSlot
       ) where

import           Universum

import           Control.Lens       (lens)

import           Pos.Core.Class     (HasEpochIndex (..), HasEpochOrSlot (..),
                                     getEpochOrSlot)
import           Pos.Core.Constants (epochSlots, slotSecurityParam)
import           Pos.Core.Types     (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                     LocalSlotIndex, SlotCount, SlotId (..), getSlotIndex,
                                     mkLocalSlotIndex)
import           Pos.Util.Util      (leftToPanic)

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: SlotId -> FlatSlotId
flattenSlotId SlotId {..} = fromIntegral $
    fromIntegral siEpoch * epochSlots +
    fromIntegral (getSlotIndex siSlot)

-- | Flattens 'EpochIndex' into a single number.
flattenEpochIndex :: EpochIndex -> FlatSlotId
flattenEpochIndex (EpochIndex i) =
    fromIntegral (fromIntegral i * epochSlots)

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: (HasEpochOrSlot a) => a -> FlatSlotId
flattenEpochOrSlot =
    epochOrSlot flattenEpochIndex flattenSlotId . getEpochOrSlot

-- | Construct 'SlotId' from a flattened variant.
unflattenSlotId :: FlatSlotId -> SlotId
unflattenSlotId n =
    let (fromIntegral -> siEpoch, fromIntegral -> slot) =
            n `divMod` fromIntegral epochSlots
        siSlot = leftToPanic "unflattenSlotId: " $ mkLocalSlotIndex slot
    in SlotId {..}

-- | Distance (in slots) between two slots. The first slot is newer, the
-- second slot is older. An epoch is considered the same as the 0th slot of
-- that epoch.
--
-- If the difference is negative, the result will be 'Nothing'.
diffEpochOrSlot :: EpochOrSlot -> EpochOrSlot -> Maybe SlotCount
diffEpochOrSlot a b
    | a' < b'   = Nothing
    | otherwise = Just (fromInteger (a' - b'))
  where
    a' = toInteger (flattenEpochOrSlot a)
    b' = toInteger (flattenEpochOrSlot b)

instance Enum SlotId where
    toEnum = unflattenSlotId . fromIntegral
    fromEnum = fromIntegral . flattenSlotId

instance HasEpochIndex SlotId where
    epochIndexL = lens siEpoch (\s a -> s {siEpoch = a})

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: EpochIndex -> SlotId
crucialSlot 0        = SlotId {siEpoch = 0, siSlot = minBound}
crucialSlot epochIdx = SlotId {siEpoch = epochIdx - 1, ..}
  where
    siSlot =
        leftToPanic "crucialSlot: " $
        mkLocalSlotIndex (fromIntegral (epochSlots - slotSecurityParam - 1))

instance HasEpochIndex EpochOrSlot where
    epochIndexL = lens (epochOrSlot identity siEpoch) setter
      where
        setter :: EpochOrSlot -> EpochIndex -> EpochOrSlot
        setter (EpochOrSlot (Left _)) epoch = EpochOrSlot (Left epoch)
        setter (EpochOrSlot (Right slot)) epoch =
            EpochOrSlot (Right $ set epochIndexL epoch slot)

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
    fromEnum (EpochOrSlot (Left e)) =
        fromIntegral $ fromIntegral e * epochSlots + 1
    fromEnum (EpochOrSlot (Right SlotId {..})) =
        fromEnum (EpochOrSlot (Left siEpoch)) +
        fromIntegral (getSlotIndex siSlot) +
        1
    toEnum x =
        let (fromIntegral -> epoch, fromIntegral -> slot) =
                x `divMod` (fromIntegral epochSlots + 1)
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

-- | Bootstrap era is ongoing until stakes are unlocked. The reward era starts
-- from the epoch specified as the epoch that unlocks stakes:
--
-- @
--                     [unlock stake epoch]
--                             /
-- Epoch: ...  E-3  E-2  E-1   E+0  E+1  E+2  E+3  ...
--        ------------------ | -----------------------
--             Bootstrap era   Reward era
-- @
--
isBootstrapEra
    :: EpochIndex -- ^ Unlock stake epoch
    -> EpochIndex -- ^ Epoch in question (for which we determine whether it
                  --                      belongs to the bootstrap era).
    -> Bool
isBootstrapEra unlockStakeEpoch epoch =
    epoch < unlockStakeEpoch

-- | Apply one of the function depending on content of 'EpochOrSlot'.
epochOrSlot :: (EpochIndex -> a) -> (SlotId -> a) -> EpochOrSlot -> a
epochOrSlot f g = either f g . unEpochOrSlot

-- | Convert 'EpochOrSlot' to the corresponding slot. If slot is
-- stored, it's returned, otherwise 0-th slot from the stored epoch is
-- returned.
epochOrSlotToSlot :: EpochOrSlot -> SlotId
epochOrSlotToSlot = epochOrSlot (flip SlotId minBound) identity
