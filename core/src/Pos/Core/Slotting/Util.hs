{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Slotting functions.

module Pos.Core.Slotting.Util
       ( flattenSlotId
       , flattenSlotIdExplicit
       , flattenEpochIndex
       , flattenEpochOrSlot
       , unflattenSlotId
       , unflattenSlotIdExplicit
       , diffEpochOrSlot
       , flatSlotId
       , crucialSlot
       , unsafeMkLocalSlotIndexExplicit
       , unsafeMkLocalSlotIndex
       , isBootstrapEra
       , epochOrSlot
       , epochOrSlotToSlot
       , mkLocalSlotIndexExplicit
       , mkLocalSlotIndex
       , addLocalSlotIndex

       , localSlotIndexMinBound
       , localSlotIndexMaxBound
       ) where

import           Universum

import           Control.Lens (Iso', iso, lens)
import           Control.Monad.Except (MonadError (throwError))
import           System.Random (Random (..))

import           Pos.Core.Class (HasEpochIndex (..), HasEpochOrSlot (..), getEpochOrSlot)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants, epochSlots,
                                                  protocolConstants, slotSecurityParam)
import           Pos.Core.ProtocolConstants (ProtocolConstants, pcEpochSlots)
import           Pos.Core.Slotting.Types (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                          LocalSlotIndex (..), SlotCount, SlotId (..), getSlotIndex)
import           Pos.Util.Util (leftToPanic)

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: HasProtocolConstants => SlotId -> FlatSlotId
flattenSlotId = flattenSlotIdExplicit epochSlots

flattenSlotIdExplicit :: SlotCount -> SlotId -> FlatSlotId
flattenSlotIdExplicit es SlotId {..} = fromIntegral $
    fromIntegral siEpoch * es +
    fromIntegral (getSlotIndex siSlot)

-- | Flattens 'EpochIndex' into a single number.
flattenEpochIndex :: HasProtocolConstants => EpochIndex -> FlatSlotId
flattenEpochIndex (EpochIndex i) =
    fromIntegral (fromIntegral i * epochSlots)

-- | Transforms some 'HasEpochOrSlot' to a single number.
flattenEpochOrSlot :: (HasProtocolConstants, HasEpochOrSlot a) => a -> FlatSlotId
flattenEpochOrSlot =
    epochOrSlot flattenEpochIndex flattenSlotId . getEpochOrSlot

-- | Construct 'SlotId' from a flattened variant.
unflattenSlotId :: HasProtocolConstants => FlatSlotId -> SlotId
unflattenSlotId = unflattenSlotIdExplicit epochSlots

-- | Construct a 'SlotId' from a flattened variant, using a given 'SlotCount'
-- modulus.
unflattenSlotIdExplicit :: SlotCount -> FlatSlotId -> SlotId
unflattenSlotIdExplicit es n =
    let (fromIntegral -> siEpoch, fromIntegral -> slot) =
            n `divMod` fromIntegral es
        siSlot = leftToPanic "unflattenSlotId: " $ mkLocalSlotIndexThrow_ es slot
    in  SlotId {..}

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

flatSlotId :: HasProtocolConstants => Iso' SlotId FlatSlotId
flatSlotId = iso flattenSlotId unflattenSlotId

instance HasProtocolConstants => Enum SlotId where
    toEnum = unflattenSlotId . fromIntegral
    fromEnum = fromIntegral . flattenSlotId

instance HasEpochIndex SlotId where
    epochIndexL = lens siEpoch (\s a -> s {siEpoch = a})

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: HasProtocolConstants => EpochIndex -> SlotId
crucialSlot 0        = SlotId {siEpoch = 0, siSlot = minBound}
crucialSlot epochIdx = SlotId {siEpoch = epochIdx - 1, ..}
  where
    siSlot =
        leftToPanic "crucialSlot: " $
        mkLocalSlotIndex (fromIntegral (fromIntegral epochSlots - slotSecurityParam - 1))

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

instance HasProtocolConstants => Enum LocalSlotIndex where
    toEnum i | i >= fromIntegral epochSlots = error "toEnum @LocalSlotIndex: greater than maxBound"
             | i < 0 = error "toEnum @LocalSlotIndex: less than minBound"
             | otherwise = UnsafeLocalSlotIndex (fromIntegral i)
    fromEnum = fromIntegral . getSlotIndex

instance HasProtocolConstants => Random LocalSlotIndex where
    random = randomR (minBound, maxBound)
    randomR (UnsafeLocalSlotIndex lo, UnsafeLocalSlotIndex hi) g =
        let (r, g') = randomR (lo, hi) g
        in  (UnsafeLocalSlotIndex r, g')

instance HasProtocolConstants => Bounded LocalSlotIndex where
    minBound = UnsafeLocalSlotIndex 0
    maxBound = UnsafeLocalSlotIndex (fromIntegral epochSlots - 1)

localSlotIndexMinBound :: LocalSlotIndex
localSlotIndexMinBound = UnsafeLocalSlotIndex 0

localSlotIndexMaxBound :: ProtocolConstants -> LocalSlotIndex
localSlotIndexMaxBound pc = UnsafeLocalSlotIndex (fromIntegral (pcEpochSlots pc) - 1)

mkLocalSlotIndex_ :: SlotCount -> Word16 -> Maybe LocalSlotIndex
mkLocalSlotIndex_ es idx
    | idx < fromIntegral es = Just (UnsafeLocalSlotIndex idx)
    | otherwise = Nothing

mkLocalSlotIndexThrow_ :: MonadError Text m => SlotCount -> Word16 -> m LocalSlotIndex
mkLocalSlotIndexThrow_ es idx = case mkLocalSlotIndex_ es idx of
    Just it -> pure it
    Nothing -> throwError $
        "local slot is greater than or equal to the number of slots in epoch: " <>
        show idx

mkLocalSlotIndex :: (HasProtocolConstants, MonadError Text m) => Word16 -> m LocalSlotIndex
mkLocalSlotIndex = mkLocalSlotIndexThrow_ epochSlots

mkLocalSlotIndexExplicit :: MonadError Text m => ProtocolConstants -> Word16 -> m LocalSlotIndex
mkLocalSlotIndexExplicit pc = mkLocalSlotIndexThrow_ (pcEpochSlots pc)

-- | Shift slot index by given amount, and return 'Nothing' if it has
-- overflowed past 'epochSlots'.
addLocalSlotIndex :: HasProtocolConstants => SlotCount -> LocalSlotIndex -> Maybe LocalSlotIndex
addLocalSlotIndex x (UnsafeLocalSlotIndex i)
    | s < fromIntegral epochSlots = Just (UnsafeLocalSlotIndex (fromIntegral s))
    | otherwise      = Nothing
  where
    s :: Word64
    s = fromIntegral x + fromIntegral i

-- | Unsafe constructor of 'LocalSlotIndex'.
unsafeMkLocalSlotIndex :: HasProtocolConstants => Word16 -> LocalSlotIndex
unsafeMkLocalSlotIndex = unsafeMkLocalSlotIndexExplicit protocolConstants

unsafeMkLocalSlotIndexExplicit :: ProtocolConstants -> Word16 -> LocalSlotIndex
unsafeMkLocalSlotIndexExplicit pc =
    leftToPanic "unsafeMkLocalSlotIndex failed: " . mkLocalSlotIndexExplicit pc

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
epochOrSlotToSlot :: HasProtocolConstants => EpochOrSlot -> SlotId
epochOrSlotToSlot = epochOrSlot (flip SlotId minBound) identity
