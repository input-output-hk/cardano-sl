{-# LANGUAGE RecordWildCards #-}

module Pos.Core.Slotting.SlotId
       ( SlotId (..)
       , siEpochL
       , siSlotL
       , slotIdF

       , slotIdToEnum
       , slotIdFromEnum
       , slotIdSucc
       , slotIdPred

       , FlatSlotId
       , flatSlotId

       , flattenSlotId
       , flattenEpochIndex
       , unflattenSlotId

       , crucialSlot
       ) where

import           Universum

import           Control.Lens (Iso', iso, lens, makeLensesFor)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (Format, bprint, build, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Common (BlockCount)
import           Pos.Core.ProtocolConstants (kEpochSlots, kSlotSecurityParam)
import           Pos.Util.Util (intords, leftToPanic)

import           Pos.Core.Slotting.EpochIndex
import           Pos.Core.Slotting.LocalSlotIndex
import           Pos.Core.Slotting.SlotCount (SlotCount)

-- | Slot is identified by index of epoch and index of slot in
-- this epoch. This is a global index, an index to a global
-- slot position.
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic, Typeable)

instance Buildable SlotId where
    build SlotId {..} =
        bprint (intords%" slot of "%intords%" epoch")
            (getSlotIndex siSlot) (getEpochIndex siEpoch)

instance NFData SlotId

deriveJSON defaultOptions ''SlotId

deriveSafeCopySimple 0 'base ''SlotId

flip makeLensesFor ''SlotId [
    ("siEpoch", "siEpochL"),
    ("siSlot" , "siSlotL") ]

slotIdToEnum :: SlotCount -> Int -> SlotId
slotIdToEnum epochSlots = unflattenSlotId epochSlots . fromIntegral

slotIdFromEnum :: SlotCount -> SlotId -> Int
slotIdFromEnum epochSlots = fromIntegral . flattenSlotId epochSlots

slotIdSucc :: SlotCount -> SlotId -> SlotId
slotIdSucc epochSlots =
    slotIdToEnum epochSlots . (+ 1) . slotIdFromEnum epochSlots

slotIdPred :: SlotCount -> SlotId -> SlotId
slotIdPred epochSlots =
    slotIdToEnum epochSlots . subtract 1 . slotIdFromEnum epochSlots

instance HasEpochIndex SlotId where
    epochIndexL = lens siEpoch (\s a -> s {siEpoch = a})

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

-- | Flatten 'SlotId' (which is basically pair of integers) into a single number.
flattenSlotId :: SlotCount -> SlotId -> FlatSlotId
flattenSlotId es SlotId {..} =
    fromIntegral $ fromIntegral siEpoch * es + fromIntegral
        (getSlotIndex siSlot)

-- | Flattens 'EpochIndex' into a single number.
flattenEpochIndex :: SlotCount -> EpochIndex -> FlatSlotId
flattenEpochIndex epochSlots (EpochIndex i) =
    fromIntegral (fromIntegral i * epochSlots)

-- | Construct a 'SlotId' from a flattened variant, using a given 'SlotCount'
-- modulus.
unflattenSlotId :: SlotCount -> FlatSlotId -> SlotId
unflattenSlotId es n =
    let (fromIntegral -> siEpoch, fromIntegral -> slot) =
            n `divMod` fromIntegral es
        siSlot = leftToPanic "unflattenSlotId: " $ mkLocalSlotIndex es slot
    in  SlotId {..}

flatSlotId :: SlotCount -> Iso' SlotId FlatSlotId
flatSlotId epochSlots =
    iso (flattenSlotId epochSlots) (unflattenSlotId epochSlots)

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: BlockCount -> EpochIndex -> SlotId
crucialSlot _ 0        = SlotId {siEpoch = 0, siSlot = localSlotIndexMinBound}
crucialSlot k epochIdx = SlotId {siEpoch = epochIdx - 1, siSlot = siSlot}
  where
    siSlot =
        leftToPanic "crucialSlot: "
            . mkLocalSlotIndex (kEpochSlots k)
            . fromIntegral
            $ fromIntegral (kEpochSlots k)
            - (kSlotSecurityParam k)
            - 1

-- TH instances

deriveSimpleBi ''SlotId [
    Cons 'SlotId [
        Field [| siEpoch :: EpochIndex     |],
        Field [| siSlot  :: LocalSlotIndex |]
    ]]
