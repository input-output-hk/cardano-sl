{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module Test.Pos.Core.EnumEmpTest
       ( tests
       ) where

-- The types `EpochOrSlot`, `LocalSlotIndex` and `SlotId` have a
-- `HasProtocoConstants` constraint as well as `Enum` instances which depend
-- on the `HasProtocoConstants` constraint. Since we want to remove
-- `HasProtocoConstants` the first step is to provide functions that will act
-- like the `Enum` methods, but take an extra parameter for
-- the part they require from `ProtocolConstants`.

-- These tests to show the equivalence between the 'Enum/HasProtocolConstant'
-- instances for these types and the new functions that replace them. When the
-- 'Enum/HasProtocolConstant' are removed, these tests will also be removed.

import           Control.DeepSeq (force)
import           Control.Exception (ErrorCall (..))
import qualified Control.Exception as Exception
import           Control.Monad.IO.Class (liftIO)
import           Universum

import           Hedgehog (Property, PropertyT, discover, (===))
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Core (pcEpochSlots, withProtocolConstants)
import           Pos.Core.Slotting (epochOrSlotFromEnum, epochOrSlotToEnum,
                     localSlotIndexFromEnum, localSlotIndexToEnum,
                     slotIdFromEnum, slotIdToEnum)

import           Test.Pos.Core.Gen


prop_enum_eqivanent_epochOrSlotFromEnum :: Property
prop_enum_eqivanent_epochOrSlotFromEnum =
    H.withTests 5000 . H.property $ do
        pc <- H.forAll genProtocolConstants
        eos <- H.forAll $ genEpochOrSlot pc
        old <- catchErrorCall $ withProtocolConstants pc (fromEnum eos)
        new <- catchErrorCall $ epochOrSlotFromEnum (pcEpochSlots pc) eos
        old === new

prop_enum_eqivanent_epochOrSlotToEnum :: Property
prop_enum_eqivanent_epochOrSlotToEnum =
    H.withTests 5000 . H.property $ do
        i <- H.forAll $ Gen.int (Range.linear (-10) 10000)
        pc <- H.forAll genProtocolConstants
        old <- catchErrorCall $ withProtocolConstants pc (toEnum i)
        new <- catchErrorCall $ epochOrSlotToEnum (pcEpochSlots pc) i
        old === new

prop_enum_eqivanent_localSlotIndexFromEnum :: Property
prop_enum_eqivanent_localSlotIndexFromEnum =
    H.withTests 5000 . H.property $ do
        pc <- H.forAll genProtocolConstants
        lsi <- H.forAll $ genLocalSlotIndex pc
        old <- catchErrorCall $ withProtocolConstants pc (fromEnum lsi)
        new <- catchErrorCall $ localSlotIndexFromEnum lsi
        old === new

prop_enum_eqivanent_localSlotIndexToEnum :: Property
prop_enum_eqivanent_localSlotIndexToEnum =
    H.withTests 5000 . H.property $ do
        i <- H.forAll $ Gen.int (Range.linear (-10) 1000000000)
        pc <- H.forAll genProtocolConstants
        old <- catchErrorCall $ withProtocolConstants pc (toEnum i)
        new <- catchErrorCall $ localSlotIndexToEnum (pcEpochSlots pc) i
        old === new

prop_enum_eqivanent_slotIdFromEnum :: Property
prop_enum_eqivanent_slotIdFromEnum =
    H.withTests 5000 . H.property $ do
        pc <- H.forAll genProtocolConstants
        sid <- H.forAll $ genSlotId pc
        old <- catchErrorCall $ withProtocolConstants pc (fromEnum sid)
        new <- catchErrorCall $ slotIdFromEnum (pcEpochSlots pc) sid
        old === new

prop_enum_eqivanent_slotIdToEnum :: Property
prop_enum_eqivanent_slotIdToEnum =
    H.withTests 5000 . H.property $ do
        i <- H.forAll $ Gen.int (Range.linear (-10) 1000000)
        pc <- H.forAll genProtocolConstants
        old <- catchErrorCall $ withProtocolConstants pc (toEnum i)
        new <- catchErrorCall $ slotIdToEnum (pcEpochSlots pc) i
        old === new

-- | Evaluate and force a pure value and if the evaluation of the pure value
-- causes 'error' to be called, catch 'ErrorCall' and return the error message
-- with the "functionName:" part removed.
--
-- This is the stupid level of crap you have to go through when people
-- think its ok to sprinkle 'error' and 'MonadError' throughout otherwise
-- pure code.
catchErrorCall :: NFData a => a -> PropertyT IO (Either String a)
catchErrorCall =
    fmap convert . liftIO . Exception.try . Exception.evaluate . force
  where
    convert :: Either ErrorCall a -> Either String a
    convert (Left (ErrorCallWithLocation s _)) = Left $ drop 1 (dropWhile (/= ':') s)
    convert (Right x) = Right x

tests :: IO Bool
tests =
    H.checkParallel $$discover
