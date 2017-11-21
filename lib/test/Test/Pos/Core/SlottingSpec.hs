-- | Specification of Pos.Core.Slotting.

module Test.Pos.Core.SlottingSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, anyErrorCall, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (NonNegative (..), Positive (..), Property, (===), (==>))

import           Pos.Arbitrary.Core (EoSToIntOverflow (..), UnreasonableEoS (..))
import           Pos.Core (EpochOrSlot, HasConfiguration, SlotId (..), flattenSlotId,
                           unflattenSlotId)

import           Test.Pos.Helpers (shouldThrowException, (.=.))
import           Test.Pos.Util (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Slotting" $ do
    describe "SlotId" $ do
        describe "Ord" $ do
            prop "is consistent with flatten/unflatten"
                flattenOrdConsistency

        describe "flattening" $ do
            prop "unflattening after flattening returns original SlotId"
                flattenThenUnflatten

    describe "EpochOrSlot" $ do
        prop "succ . pred = id" predThenSucc
        prop "Using 'pred' with 'minBound :: EpochOrSlot' triggers an exception"
            predToMinBound
        prop "pred . succ = id" succThenPred
        prop "Using 'succ' with 'maxBound :: EpochOrSlot' triggers an exception"
            succToMaxBound
        prop "from . toEnum = id @Int" toFromEnum
        prop "toEnum . fromEnum = id @EpochOrSlot" fromToEnum
        prop "toEnum . fromEnum = id @EpochOrSlot (with very large, larger than \
             \ 'maxReasonableEpoch', epochs" fromToEnumLargeEpoch
        prop "calling 'fromEnum' with a result greater than 'maxBound :: Int' will raise \
              \ an exception" fromEnumOverflow
        prop "calling 'toEnum' with a negative number will raise an exception"
            toEnumNegative

flattenOrdConsistency :: HasConfiguration => SlotId -> SlotId -> Property
flattenOrdConsistency a b = a `compare` b === flattenSlotId a `compare` flattenSlotId b

flattenThenUnflatten :: HasConfiguration => SlotId -> Property
flattenThenUnflatten si = si === unflattenSlotId (flattenSlotId si)

predThenSucc :: HasConfiguration => EpochOrSlot -> Property
predThenSucc eos = eos > minBound ==> succ (pred eos) === eos

predToMinBound :: HasConfiguration => Expectation
predToMinBound =
    shouldThrowException pred anyErrorCall (minBound :: EpochOrSlot)

succThenPred :: HasConfiguration => EpochOrSlot -> Property
succThenPred eos = eos < maxBound ==> pred (succ eos) === eos

succToMaxBound :: HasConfiguration => Expectation
succToMaxBound = shouldThrowException succ anyErrorCall (maxBound :: EpochOrSlot)

-- It is not necessary to check that 'int < fromEnum (maxBound :: EpochOrSlot)' because
-- this is not possible with the current implementation of the type.
toFromEnum :: HasConfiguration => NonNegative Int -> Property
toFromEnum (getNonNegative -> int) = fromEnum (toEnum @EpochOrSlot int) === int

fromToEnum :: HasConfiguration => EpochOrSlot -> Property
fromToEnum = toEnum . fromEnum .=. identity

fromToEnumLargeEpoch :: HasConfiguration => UnreasonableEoS -> Property
fromToEnumLargeEpoch (getUnreasonable -> eos) = toEnum (fromEnum eos) === eos

fromEnumOverflow :: HasConfiguration => EoSToIntOverflow-> Expectation
fromEnumOverflow (getEoS -> eos) =
    shouldThrowException (fromEnum @EpochOrSlot) anyErrorCall eos

toEnumNegative :: HasConfiguration => Positive Int -> Expectation
toEnumNegative (negate . getPositive -> int) =
    shouldThrowException (toEnum @EpochOrSlot) anyErrorCall int
