-- | This module tests Binary instances for 'Pos.Explorer' types.

module Test.Pos.Explorer.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Explorer.Core (TxExtra)

import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Explorer.Arbitrary ()

spec :: Spec
spec = describe "Explorer types" $ do
    describe "Bi instances" $ do
        binaryTest @TxExtra
