{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Pos.Core.Slotting.EpochOrSlot
       ( EpochOrSlot (..)
       , HasEpochOrSlot (..)

       , flattenEpochOrSlot

       , diffEpochOrSlot

       , epochOrSlot
       , epochOrSlotG
       , epochOrSlotToSlot
       ) where

import           Universum

import           Control.Lens (Getter, lens, to)
import qualified Data.Text.Buildable as Buildable
import           Pos.Util.Some (Some, applySome)

import           Pos.Core.Configuration.Protocol (HasProtocolConstants, epochSlots)
import           Pos.Util.Util (leftToPanic)

import           Pos.Core.Slotting.EpochIndex
import           Pos.Core.Slotting.LocalSlotIndex
import           Pos.Core.Slotting.SlotCount (SlotCount)
import           Pos.Core.Slotting.SlotId

-- | Represents SlotId or EpochIndex. Useful because genesis blocks
-- have only EpochIndex, while main blocks have SlotId.
newtype EpochOrSlot = EpochOrSlot
    { unEpochOrSlot :: Either EpochIndex SlotId
    } deriving (Show, Eq, Generic, NFData)

instance Ord EpochOrSlot where
    compare (EpochOrSlot e1) (EpochOrSlot e2) = case (e1,e2) of
        (Left s1, Left s2)                      -> compare s1 s2
        (Right s1, Left s2) | (siEpoch s1) < s2 -> LT
                            | otherwise         -> GT
        (Left s1, Right s2) | s1 > (siEpoch s2) -> GT
                            | otherwise         -> LT
        (Right s1, Right s2)
            | siEpoch s1 == siEpoch s2 -> siSlot s1 `compare` siSlot s2
            | otherwise -> siEpoch s1 `compare` siEpoch s2

instance Buildable EpochOrSlot where
    build = either Buildable.build Buildable.build . unEpochOrSlot

instance HasEpochIndex EpochOrSlot where
    epochIndexL = lens (epochOrSlot identity siEpoch) setter
      where
        setter :: EpochOrSlot -> EpochIndex -> EpochOrSlot
        setter (EpochOrSlot (Left _)) epoch = EpochOrSlot (Left epoch)
        setter (EpochOrSlot (Right slot)) epoch =
            EpochOrSlot (Right $ set epochIndexL epoch slot)

instance HasProtocolConstants => Enum EpochOrSlot where
    succ (EpochOrSlot (Left e)) =
        EpochOrSlot (Right SlotId {siEpoch = e, siSlot = minBound})
    succ e@(EpochOrSlot (Right si@SlotId {..}))
        | e == maxBound = error "succ@EpochOrSlot: maxBound"
        | siSlot == maxBound = EpochOrSlot (Left (siEpoch + 1))
        | otherwise = EpochOrSlot $ Right si {siSlot = succ siSlot}
    pred eos@(EpochOrSlot (Left e))
        | eos == minBound = error "pred@EpochOrSlot: minBound"
        | otherwise =
            EpochOrSlot (Right SlotId {siEpoch = e - 1, siSlot = maxBound})
    pred (EpochOrSlot (Right si@SlotId {..}))
        | siSlot == minBound = EpochOrSlot (Left siEpoch)
        | otherwise = EpochOrSlot $ Right si {siSlot = pred siSlot}
    fromEnum (EpochOrSlot (Left e)) =
        let res = toInteger e * toInteger (epochSlots + 1)
            maxIntAsInteger = toInteger (maxBound :: Int)
        in if | res > maxIntAsInteger ->
                  error "fromEnum @EpochOrSlot: Argument larger than 'maxBound :: Int'"
              | otherwise -> fromIntegral res
    fromEnum (EpochOrSlot (Right SlotId {..})) =
        let res = toInteger (fromEnum (EpochOrSlot (Left siEpoch))) +
                  toInteger (getSlotIndex siSlot) +
                  1
            maxIntAsInteger = toInteger (maxBound :: Int)
        in if | res > maxIntAsInteger ->
                  error "fromEnum @EpochOrSlot: Argument larger than 'maxBound :: Int'"
              | otherwise -> fromIntegral res
    toEnum x =
        let (fromIntegral -> epoch, fromIntegral -> slot) =
                x `divMod` (fromIntegral epochSlots + 1)
            slotIdx =
                leftToPanic "toEnum @EpochOrSlot" $ mkLocalSlotIndex (slot - 1)
        in if | x < 0 -> error "toEnum @EpochOrSlot: Negative argument"
              | slot == 0 -> EpochOrSlot (Left epoch)
              | otherwise ->
                  EpochOrSlot (Right SlotId {siSlot = slotIdx, siEpoch = epoch})

instance HasProtocolConstants => Bounded EpochOrSlot where
    maxBound = EpochOrSlot (Right SlotId {siSlot = maxBound, siEpoch = maxBound})
    minBound = EpochOrSlot (Left (EpochIndex 0))

class HasEpochOrSlot a where
    getEpochOrSlot :: a -> EpochOrSlot

instance HasEpochOrSlot (Some HasEpochOrSlot) where
    getEpochOrSlot = applySome getEpochOrSlot

epochOrSlotG :: HasEpochOrSlot a => Getter a EpochOrSlot
epochOrSlotG = to getEpochOrSlot

instance HasEpochOrSlot EpochIndex where
    getEpochOrSlot = EpochOrSlot . Left

instance HasEpochOrSlot SlotId where
    getEpochOrSlot = EpochOrSlot . Right

instance HasEpochOrSlot EpochOrSlot where
    getEpochOrSlot = identity

instance (HasEpochOrSlot a, HasEpochOrSlot b) =>
         HasEpochOrSlot (Either a b) where
    getEpochOrSlot = either getEpochOrSlot getEpochOrSlot

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: (HasProtocolConstants, HasEpochOrSlot a) => a -> FlatSlotId
flattenEpochOrSlot =
    epochOrSlot flattenEpochIndex flattenSlotId . getEpochOrSlot

-- | Distance (in slots) between two slots. The first slot is newer, the
-- second slot is older. An epoch is considered the same as the 0th slot of
-- that epoch.
--
-- If the difference is negative, the result will be 'Nothing'.
diffEpochOrSlot :: HasProtocolConstants => EpochOrSlot -> EpochOrSlot -> Maybe SlotCount
diffEpochOrSlot a b
    | a' < b'   = Nothing
    | otherwise = Just (fromInteger (a' - b'))
  where
    a' = toInteger (flattenEpochOrSlot a)
    b' = toInteger (flattenEpochOrSlot b)


-- | Apply one of the function depending on content of 'EpochOrSlot'.
epochOrSlot :: (EpochIndex -> a) -> (SlotId -> a) -> EpochOrSlot -> a
epochOrSlot f g = either f g . unEpochOrSlot

-- | Convert 'EpochOrSlot' to the corresponding slot. If slot is
-- stored, it's returned, otherwise 0-th slot from the stored epoch is
-- returned.
epochOrSlotToSlot :: HasProtocolConstants => EpochOrSlot -> SlotId
epochOrSlotToSlot = epochOrSlot (flip SlotId minBound) identity
