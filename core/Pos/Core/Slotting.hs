{-# LANGUAGE Rank2Types #-}

-- | Slotting functions.

module Pos.Core.Slotting
       ( flattenSlotId
       , flattenEpochIndex
       , unflattenSlotId
       , flatSlotIso
       , crucialSlot
       , isBootstrapEra

       -- * EpochOrSlot
       , epochOrSlot
       , epochOrSlotToSlot
       , flattenEpochOrSlot
       , diffEpochOrSlot

       -- ** Enum (modified)
       , succEOS
       , predEOS
       , eosToInt
       , intToEOS

       -- ** Bounded (modified)
       , maxEOS
       , minEOS
       ) where

import           Universum

import           Control.Lens       (Iso', iso, lens)

import           Pos.Core.Class     (HasEpochIndex (..), HasEpochOrSlot (..),
                                     getEpochOrSlot)
import           Pos.Core.Constants (epochSlots, slotSecurityParam)
import           Pos.Core.Types     (BlockCount, EpochIndex (..), EpochOrSlot (..),
                                     FlatSlotId, LocalSlotIndex (..), SlotCount,
                                     SlotId (..), getSlotIndex)

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: BlockCount -> SlotId -> FlatSlotId
flattenSlotId k SlotId {..} = fromIntegral $
    fromIntegral siEpoch * epochSlots k +
    fromIntegral (getSlotIndex siSlot)

-- | Flattens 'EpochIndex' into a single number.
flattenEpochIndex :: BlockCount -> EpochIndex -> FlatSlotId
flattenEpochIndex k (EpochIndex i) =
    fromIntegral (fromIntegral i * epochSlots k)

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: (HasEpochOrSlot a) => BlockCount -> a -> FlatSlotId
flattenEpochOrSlot k =
    epochOrSlot (flattenEpochIndex k) (flattenSlotId k) . getEpochOrSlot

-- | Construct 'SlotId' from a flattened variant.
unflattenSlotId :: BlockCount -> FlatSlotId -> SlotId
unflattenSlotId k n =
    let (fromIntegral -> siEpoch, fromIntegral -> slot) =
            n `divMod` fromIntegral (epochSlots k)
        siSlot = LocalSlotIndex slot
    in SlotId {..}

-- | Isomorphism between 'SlotId' and 'FlatSlotId'.
flatSlotIso :: BlockCount -> Iso' SlotId FlatSlotId
flatSlotIso k = iso (flattenSlotId k) (unflattenSlotId k)

-- | Distance (in slots) between two slots. The first slot is newer, the
-- second slot is older. An epoch is considered the same as the 0th slot of
-- that epoch.
--
-- If the difference is negative, the result will be 'Nothing'.
diffEpochOrSlot :: BlockCount -> EpochOrSlot -> EpochOrSlot -> Maybe SlotCount
diffEpochOrSlot k a b
    | a' < b'   = Nothing
    | otherwise = Just (fromInteger (a' - b'))
  where
    a' = toInteger (flattenEpochOrSlot k a)
    b' = toInteger (flattenEpochOrSlot k b)

-- instance Enum SlotId where
--     toEnum = unflattenSlotId . fromIntegral
--     fromEnum = fromIntegral . flattenSlotId

instance HasEpochIndex SlotId where
    epochIndexL = lens siEpoch (\s a -> s {siEpoch = a})

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: BlockCount -> EpochIndex -> SlotId
crucialSlot _ 0                       = SlotId {siEpoch = 0, siSlot = minBound}
crucialSlot blkSecurityParam epochIdx = SlotId {siEpoch = epochIdx - 1, ..}
  where
    slotSecPar = slotSecurityParam blkSecurityParam
    siSlot = fromIntegral (epochSlots blkSecurityParam - slotSecPar - 1)

instance HasEpochIndex EpochOrSlot where
    epochIndexL = lens (epochOrSlot identity siEpoch) setter
      where
        setter :: EpochOrSlot -> EpochIndex -> EpochOrSlot
        setter (EpochOrSlot (Left _)) epoch = EpochOrSlot (Left epoch)
        setter (EpochOrSlot (Right slot)) epoch =
            EpochOrSlot (Right $ set epochIndexL epoch slot)

succEOS :: BlockCount -> EpochOrSlot -> EpochOrSlot
succEOS _ (EpochOrSlot (Left e)) =
    EpochOrSlot (Right SlotId {siEpoch = e, siSlot = 0})
succEOS k e@(EpochOrSlot (Right si@SlotId {..}))
    | e == maxEOS k = error "succEOS: maxBound"
    | fromIntegral siSlot == epochSlots k - 1 = EpochOrSlot (Left (siEpoch + 1))
    | otherwise = EpochOrSlot $ Right si {siSlot = succ siSlot}

predEOS :: BlockCount -> EpochOrSlot -> EpochOrSlot
predEOS k eos@(EpochOrSlot (Left e))
    | eos == minEOS = error "predEOS: minBound"
    | otherwise =
        EpochOrSlot (Right SlotId {siEpoch = e - 1,
                                   siSlot = fromIntegral $ epochSlots k - 1})
predEOS _ (EpochOrSlot (Right si@SlotId {..}))
    | siSlot == 0 = EpochOrSlot (Left siEpoch)
    | otherwise = EpochOrSlot $ Right si {siSlot = siSlot - 1}

eosToInt :: BlockCount -> EpochOrSlot -> Int
eosToInt k (EpochOrSlot (Left e)) =
    let res = toInteger e * toInteger (epochSlots k + 1)
        maxIntAsInteger = toInteger (maxBound :: Int)
    in if | res > maxIntAsInteger ->
                error "eosToInt: Argument larger than 'maxBound :: Int'"
            | otherwise -> fromIntegral res
eosToInt k (EpochOrSlot (Right SlotId {..})) =
    let res = toInteger (eosToInt k (EpochOrSlot (Left siEpoch))) +
                toInteger siSlot +
                1
        maxIntAsInteger = toInteger (maxBound :: Int)
    in if | res > maxIntAsInteger ->
                error "eosToInt: Argument larger than 'maxBound :: Int'"
            | otherwise -> fromIntegral res

intToEOS :: BlockCount -> Int -> EpochOrSlot
intToEOS k x =
    let (fromIntegral -> epoch, fromIntegral -> slot) =
            x `divMod` (fromIntegral (epochSlots k) + 1)
        slotIdx = slot - 1
    in if | x < 0 -> error "intToEOS: Negative argument"
            | slot == 0 -> EpochOrSlot (Left epoch)
            | otherwise ->
                EpochOrSlot (Right SlotId {siSlot = slotIdx, siEpoch = epoch})

maxEOS :: BlockCount -> EpochOrSlot
maxEOS k =
    EpochOrSlot
        (Right
             SlotId
             {siSlot = fromIntegral $ epochSlots k - 1, siEpoch = maxBound})

minEOS :: EpochOrSlot
minEOS = EpochOrSlot (Left (EpochIndex 0))

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
