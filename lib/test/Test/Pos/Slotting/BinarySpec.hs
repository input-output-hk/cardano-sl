module Test.Pos.Slotting.BinarySpec where

import           Universum

import           Test.Hspec          (Spec, describe)

import           Pos.Arbitrary.Infra ()
import           Pos.Slotting.Types  (SlottingData)

import           Test.Pos.Util       (binaryTest)


spec :: Spec
spec = describe "Slotting types" $ do
    binaryTest @SlottingData
