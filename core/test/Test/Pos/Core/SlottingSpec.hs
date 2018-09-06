-- | Specification of Pos.Core.Slotting.

module Test.Pos.Core.SlottingSpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Expectation, Spec, anyErrorCall, describe, runIO)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (NonNegative (..), Positive (..), Property, arbitrary, generate,
                                  (===), (==>))

import           Pos.Core (EpochOrSlot, HasConfiguration, SlotId (..), defaultCoreConfiguration,
                           flattenSlotId, unflattenSlotId, withGenesisSpec)
import           Pos.Crypto (ProtocolMagic (..), RequiresNetworkMagic (..))

import           Test.Pos.Core.Arbitrary (EoSToIntOverflow (..), UnreasonableEoS (..))
import           Test.Pos.Util.QuickCheck.Property (shouldThrowException, (.=.))

spec :: Spec
spec = do
    runWithMagic NMMustBeNothing
    runWithMagic NMMustBeJust

runWithMagic :: RequiresNetworkMagic -> Spec
runWithMagic rnm = do
    pm <- (\ident -> ProtocolMagic ident rnm) <$> runIO (generate arbitrary)
    describe ("(requiresNetworkMagic=" ++ show rnm ++ ")") $
        specBody pm

specBody :: ProtocolMagic -> Spec
specBody pm = withGenesisSpec 0 (defaultCoreConfiguration pm) $ \_ -> describe "Slotting" $ do
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
