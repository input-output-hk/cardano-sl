-- | Arbitrary instances for core.

module Pos.Types.Arbitrary.Core
       (
       ) where

import           Universum

import           System.Random        (Random)
import           Test.QuickCheck      (Arbitrary (..), choose, oneof)

import           Pos.Core.Types       (getSlotIndex, mkLocalSlotIndex)
import qualified Pos.Core.Types       as Types
import           Pos.Crypto.Arbitrary ()
import           Pos.Util.Util        (leftToPanic)

maxReasonableEpoch :: Integral a => a
maxReasonableEpoch = 5 * 1000 * 1000 * 1000 * 1000  -- 5 * 10^12, because why not

deriving instance Random Types.EpochIndex

instance Arbitrary Types.EpochIndex where
    arbitrary = choose (0, maxReasonableEpoch)

instance Arbitrary Types.LocalSlotIndex where
    arbitrary =
        leftToPanic "arbitrary@LocalSlotIndex: " . mkLocalSlotIndex <$>
        choose (getSlotIndex minBound, getSlotIndex maxBound)

instance Arbitrary Types.SlotId where
    arbitrary = Types.SlotId
        <$> arbitrary
        <*> arbitrary

instance Arbitrary Types.EpochOrSlot where
    arbitrary = oneof [
          Types.EpochOrSlot . Left <$> arbitrary
        , Types.EpochOrSlot . Right <$> arbitrary
        ]
