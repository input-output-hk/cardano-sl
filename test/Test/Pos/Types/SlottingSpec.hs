-- | Specification of Pos.Types.Slotting.

module Test.Pos.Types.SlottingSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec            (Expectation, Spec, anyErrorCall, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (NonNegative (..), Positive (..), Property, (===),
                                        (==>))

import           Pos.Arbitrary.Core    (EoSToIntOverflow (..), UnreasonableEoS (..))
import           Pos.Types             (EpochOrSlot, SlotId (..), flattenSlotId,
                                        unflattenSlotId)
import           Test.Pos.Util         (shouldThrowException, (.=.))

spec :: Spec
spec = describe "Slotting" $ do
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

flattenOrdConsistency :: SlotId -> SlotId -> Property
flattenOrdConsistency a b = a `compare` b === flattenSlotId a `compare` flattenSlotId b

flattenThenUnflatten :: SlotId -> Property
flattenThenUnflatten si = si === unflattenSlotId (flattenSlotId si)

predThenSucc :: EpochOrSlot -> Property
predThenSucc eos = eos > minBound ==> succ (pred eos) === eos

predToMinBound :: Expectation
predToMinBound =
    shouldThrowException pred anyErrorCall (minBound :: EpochOrSlot)

succThenPred :: EpochOrSlot -> Property
succThenPred eos = eos < maxBound ==> pred (succ eos) === eos

succToMaxBound :: Expectation
succToMaxBound = shouldThrowException succ anyErrorCall (maxBound :: EpochOrSlot)

-- It is not necessary to check that 'int < fromEnum (maxBound :: EpochOrSlot)' because
-- this is not possible with the current implementation of the type.
toFromEnum :: NonNegative Int -> Property
toFromEnum (getNonNegative -> int) = fromEnum (toEnum @EpochOrSlot int) === int

fromToEnum :: EpochOrSlot -> Property
fromToEnum = toEnum . fromEnum .=. identity

fromToEnumLargeEpoch :: UnreasonableEoS -> Property
fromToEnumLargeEpoch (getUnreasonable -> eos) = toEnum (fromEnum eos) === eos

fromEnumOverflow :: EoSToIntOverflow-> Expectation
fromEnumOverflow (getEoS -> eos) =
    shouldThrowException (fromEnum @EpochOrSlot) anyErrorCall eos

toEnumNegative :: Positive Int -> Expectation
toEnumNegative (negate . getPositive -> int) =
    shouldThrowException (toEnum @EpochOrSlot) anyErrorCall int
