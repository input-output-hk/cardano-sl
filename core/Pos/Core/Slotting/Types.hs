-- | Core slotting types.

module Pos.Core.Slotting.Types
       (
         EpochIndex (..)
       , FlatSlotId
       , LocalSlotIndex (..)
       , SlotId (..)
       , siEpochL
       , siSlotL
       , slotIdF
       , EpochOrSlot (..)
       , SlotCount (..)

       -- * Re-exported
       , Timestamp (..)
       , TimeDiff (..)
       ) where

import           Universum

import           Control.Lens (makeLensesFor)
import           Data.Ix (Ix)
import qualified Data.Text.Buildable as Buildable
import           Formatting (Format, bprint, build, int, ords, (%))
import           System.Random (Random (..))

import           Pos.Core.Slotting.Timestamp (TimeDiff (..), Timestamp (..))

-- | Index of epoch.
newtype EpochIndex = EpochIndex
    { getEpochIndex :: Word64
    } deriving (Show, Eq, Ord, Num, Enum, Ix, Integral, Real, Generic, Hashable, Bounded, Typeable, NFData)

instance Buildable EpochIndex where
    build = bprint ("#"%int)

-- | Index of slot inside a concrete epoch.
newtype LocalSlotIndex = UncheckedLocalSlotIndex
    { getSlotIndex :: Word16
    } deriving (Show, Eq, Ord, Ix, Generic, Hashable, Buildable, Typeable, NFData)

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

-- | Specialized formatter for 'SlotId'.
slotIdF :: Format r (SlotId -> r)
slotIdF = build

-- | FlatSlotId is a flat version of SlotId
type FlatSlotId = Word64

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

instance NFData SlotId

flip makeLensesFor ''SlotId [
    ("siEpoch", "siEpochL"),
    ("siSlot" , "siSlotL") ]

newtype SlotCount = SlotCount {getSlotCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)
