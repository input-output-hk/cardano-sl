-- | Specification of Pos.Core.Slotting.

module Test.Pos.Core.SlottingSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, anyErrorCall, describe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (NonNegative (..), Positive (..), Property,
                     (===), (==>))

import           Pos.Core (EpochOrSlot, SlotId (..), epochOrSlotFromEnum,
                     epochOrSlotMaxBound, epochOrSlotMinBound, epochOrSlotPred,
                     epochOrSlotSucc, epochOrSlotToEnum, flattenSlotId,
                     unflattenSlotId)

import           Test.Pos.Core.Arbitrary (EoSToIntOverflow (..),
                     UnreasonableEoS (..))
import           Test.Pos.Core.Dummy (dummyEpochSlots)
import           Test.Pos.Util.QuickCheck.Property (shouldThrowException, (.=.))

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
flattenOrdConsistency a b =
    a
        `compare` b
        ===       flattenSlotId dummyEpochSlots a
        `compare` flattenSlotId dummyEpochSlots b

flattenThenUnflatten :: SlotId -> Property
flattenThenUnflatten si =
    si === unflattenSlotId dummyEpochSlots (flattenSlotId dummyEpochSlots si)

predThenSucc :: EpochOrSlot -> Property
predThenSucc eos =
    eos
        >   epochOrSlotMinBound
        ==> epochOrSlotSucc dummyEpochSlots
                            (epochOrSlotPred dummyEpochSlots eos)
        === eos

predToMinBound :: Expectation
predToMinBound = shouldThrowException (epochOrSlotPred dummyEpochSlots)
                                      anyErrorCall
                                      epochOrSlotMinBound

succThenPred :: EpochOrSlot -> Property
succThenPred eos =
    eos
        <   epochOrSlotMaxBound dummyEpochSlots
        ==> epochOrSlotPred dummyEpochSlots
                            (epochOrSlotSucc dummyEpochSlots eos)
        === eos

succToMaxBound :: Expectation
succToMaxBound = shouldThrowException (epochOrSlotSucc dummyEpochSlots)
                                      anyErrorCall
                                      (epochOrSlotMaxBound dummyEpochSlots)

-- It is not necessary to check that 'int < fromEnum (maxBound :: EpochOrSlot)' because
-- this is not possible with the current implementation of the type.
toFromEnum :: NonNegative Int -> Property
toFromEnum (getNonNegative -> int) =
    epochOrSlotFromEnum dummyEpochSlots (epochOrSlotToEnum dummyEpochSlots int)
        === int

fromToEnum :: EpochOrSlot -> Property
fromToEnum =
    epochOrSlotToEnum dummyEpochSlots
        .   epochOrSlotFromEnum dummyEpochSlots
        .=. identity

fromToEnumLargeEpoch :: UnreasonableEoS -> Property
fromToEnumLargeEpoch (getUnreasonable -> eos) =
    epochOrSlotToEnum dummyEpochSlots (epochOrSlotFromEnum dummyEpochSlots eos)
        === eos

fromEnumOverflow :: EoSToIntOverflow -> Expectation
fromEnumOverflow (getEoS -> eos) =
    shouldThrowException (epochOrSlotFromEnum dummyEpochSlots) anyErrorCall eos

toEnumNegative :: Positive Int -> Expectation
toEnumNegative (negate . getPositive -> int) =
    shouldThrowException (epochOrSlotToEnum dummyEpochSlots) anyErrorCall int
