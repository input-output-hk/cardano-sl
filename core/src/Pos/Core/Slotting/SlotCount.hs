module Pos.Core.Slotting.SlotCount
       ( SlotCount (..)
       ) where

import           Universum

import           System.Random (Random (..))

newtype SlotCount = SlotCount {getSlotCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)
