module Pos.Core.Slotting.SlotCount
       ( SlotCount (..)
       ) where

import           Universum

import           Data.Aeson (FromJSON (..), ToJSON (..))
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           System.Random (Random (..))

import           Pos.Binary.Class (Bi (..))

-- | This type actually contains the number of slots per epoch, but the people
-- who wrote this code hate us!
newtype SlotCount = SlotCount {getSlotCount :: Word64}
    deriving (Eq, Ord, Num, Real, Integral, Enum, Read, Show,
              Buildable, Generic, Typeable, NFData, Hashable, Random)

instance Bi SlotCount where
    encode = encode . getSlotCount
    decode = SlotCount <$> decode

deriving instance ToJSON SlotCount
deriving instance FromJSON SlotCount

deriveSafeCopySimple 0 'base ''SlotCount
