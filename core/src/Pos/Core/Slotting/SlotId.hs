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
       , flattenSlotIdExplicit
       , flattenEpochIndex
       , unflattenSlotId
       , unflattenSlotIdExplicit

       , crucialSlot
       ) where

import           Universum

import           Control.Lens (Iso', iso, lens, makeLensesFor)
import           Data.Aeson.TH (defaultOptions, deriveJSON)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (Format, bprint, build, ords, (%))
import qualified Formatting.Buildable as Buildable

import           Pos.Binary.Class (Cons (..), Field (..), deriveSimpleBi)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants,
                     epochSlots, slotSecurityParam)
import           Pos.Util.Util (leftToPanic)

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
        bprint (ords%" slot of "%ords%" epoch") (getSlotIndex siSlot) siEpoch

instance NFData SlotId

deriveJSON defaultOptions ''SlotId

deriveSafeCopySimple 0 'base ''SlotId

flip makeLensesFor ''SlotId [
    ("siEpoch", "siEpochL"),
    ("siSlot" , "siSlotL") ]

instance HasProtocolConstants => Enum SlotId where
    toEnum = unflattenSlotId . fromIntegral
    fromEnum = fromIntegral . flattenSlotId

slotIdToEnum :: SlotCount -> Int -> SlotId
slotIdToEnum es = unflattenSlotIdExplicit es . fromIntegral

slotIdFromEnum :: SlotCount -> SlotId -> Int
slotIdFromEnum es = fromIntegral . flattenSlotIdExplicit es

slotIdSucc :: SlotCount -> SlotId -> SlotId
slotIdSucc es =
    slotIdToEnum es . (+ 1) . slotIdFromEnum es

slotIdPred :: SlotCount -> SlotId -> SlotId
slotIdPred es =
    slotIdToEnum es . subtract 1 . slotIdFromEnum es

instance HasEpochIndex SlotId where
    epochIndexL = lens siEpoch (\s a -> s {siEpoch = a})

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

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

-- | Construct 'SlotId' from a flattened variant.
unflattenSlotId :: HasProtocolConstants => FlatSlotId -> SlotId
unflattenSlotId = unflattenSlotIdExplicit epochSlots

-- | Construct a 'SlotId' from a flattened variant, using a given 'SlotCount'
-- modulus.
unflattenSlotIdExplicit :: SlotCount -> FlatSlotId -> SlotId
unflattenSlotIdExplicit es n =
    let (fromIntegral -> siEpoch, fromIntegral -> slot) =
            n `divMod` fromIntegral es
        siSlot = leftToPanic "unflattenSlotIdExplicit: " $ mkLocalSlotIndexThrow_ es slot
    in  SlotId {..}

flatSlotId :: HasProtocolConstants => Iso' SlotId FlatSlotId
flatSlotId = iso flattenSlotId unflattenSlotId

-- | Slot such that at the beginning of epoch blocks with SlotId ≤- this slot
-- are stable.
crucialSlot :: HasProtocolConstants => EpochIndex -> SlotId
crucialSlot 0        = SlotId {siEpoch = 0, siSlot = minBound}
crucialSlot epochIdx = SlotId {siEpoch = epochIdx - 1, ..}
  where
    siSlot =
        leftToPanic "crucialSlot: " $
        mkLocalSlotIndex (fromIntegral (fromIntegral epochSlots - slotSecurityParam - 1))

-- TH instances

deriveSimpleBi ''SlotId [
    Cons 'SlotId [
        Field [| siEpoch :: EpochIndex     |],
        Field [| siSlot  :: LocalSlotIndex |]
    ]]
