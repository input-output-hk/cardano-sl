module Pos.Core.Slotting.SlotCount
       ( SlotCount (..)
       ) where

import           Universum

import           Data.Aeson (ToJSON (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           System.Random (Random (..))

import           Pos.Binary.Class (Bi (..))

newtype SlotCount = SlotCount {getSlotCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

instance Bi SlotCount where
    encode = encode . getSlotCount
    decode = SlotCount <$> decode

deriving instance ToJSON SlotCount

deriveSafeCopySimple 0 'base ''SlotCount
