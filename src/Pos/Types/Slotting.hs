{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

-- | Slotting types.

module Pos.Types.Slotting
       ( EpochIndex (..)
       , LocalSlotIndex (..)

       , flattenSlotId
       , unflattenSlotId

       , SlotId (..)
       , slotIdF
       , FlatSlotId
       ) where

import           Data.Binary         (Binary)
import           Data.Hashable       (Hashable)
import           Data.Ix             (Ix)
import           Data.MessagePack    (MessagePack)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, ords, (%))
import           Pos.Constants       (epochSlots)
import           Universum

----------------------------------------------------------------------------
-- Slotting
----------------------------------------------------------------------------

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Integral, Real, Generic, Binary, Hashable, Buildable)

instance MessagePack EpochIndex

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = LocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Binary, Hashable, Buildable)

instance MessagePack LocalSlotIndex

-- | Slot is identified by index of epoch and local index of slot in
-- this epoch. This is a global index
data SlotId = SlotId
    { siEpoch :: !EpochIndex
    , siSlot  :: !LocalSlotIndex
    } deriving (Show, Eq, Ord, Generic)

instance Binary SlotId
instance MessagePack SlotId

instance Buildable SlotId where
    build SlotId {..} =
        bprint (ords%" slot of "%ords%" epoch") siSlot siEpoch

slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64


-- | Flatten SlotId (which is basically pair of integers) into a single number.
flattenSlotId :: SlotId -> FlatSlotId
flattenSlotId SlotId {..} = fromIntegral siEpoch * epochSlots + fromIntegral siSlot

-- | Construct SlotId from a flattened variant.
-- TODO: what is antonym of word `flatten`?
unflattenSlotId :: FlatSlotId -> SlotId
unflattenSlotId n =
    let (fromIntegral -> siEpoch, fromIntegral -> siSlot) =
            n `divMod` epochSlots
    in SlotId {..}

instance Enum SlotId where
    toEnum = unflattenSlotId . fromIntegral
    fromEnum = fromIntegral . flattenSlotId
