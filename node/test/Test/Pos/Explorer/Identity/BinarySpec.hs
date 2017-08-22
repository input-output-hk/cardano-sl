-- | This module tests Binary instances for 'Pos.Explorer' types.

module Test.Pos.Explorer.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec     (Spec, describe)

import           Pos.Explorer   (TxExtra)
import           Test.Pos.Util  (binaryTest)


spec :: Spec
spec = describe "Explorer types" $ do
    describe "Bi instances" $ do
        binaryTest @TxExtra
