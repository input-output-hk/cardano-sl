-- | Arbitrary instances for core.

module Pos.Types.Arbitrary.Core
       (
       ) where

import           Universum

import           System.Random                     (Random)
import           Test.QuickCheck                   (Arbitrary (..), choose, oneof)
import           Test.QuickCheck.Arbitrary.Generic (genericArbitrary, genericShrink)

import qualified Pos.Core.Fee                      as Fee
import           Pos.Core.Types                    (getSlotIndex, mkLocalSlotIndex)
import qualified Pos.Core.Types                    as Types
import           Pos.Crypto.Arbitrary              ()
import           Pos.Util.Util                     (leftToPanic)

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

deriving instance Arbitrary Fee.Coeff

instance Arbitrary Fee.TxSizeLinear where
    arbitrary = genericArbitrary
    shrink = genericShrink

instance Arbitrary Fee.TxFeePolicy where
    arbitrary = oneof
        [ Fee.TxFeePolicyTxSizeLinear <$> arbitrary
        , do
              policyCode <-
                  -- The lower bound is needed so that
                  -- we don't get codes for known policies.
                  choose (1, maxBound)
              policyPayload <- arbitrary
              return $ Fee.TxFeePolicyUnknown policyCode policyPayload
        ]
    shrink = \case
        Fee.TxFeePolicyTxSizeLinear a ->
            Fee.TxFeePolicyTxSizeLinear <$> shrink a
        Fee.TxFeePolicyUnknown v a ->
            Fee.TxFeePolicyUnknown v <$> shrink a
