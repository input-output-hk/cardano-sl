-- | This module tests Binary instances for 'Pos.Explorer' types.

module Test.Pos.Explorer.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Test.Hspec (Spec, describe)

import           Pos.Arbitrary.Explorer ()
import           Pos.Explorer.Core (TxExtra)
import           Test.Pos.Binary.Helpers (binaryTest)
import           Test.Pos.Configuration (withDefConfiguration)

spec :: Spec
spec = withDefConfiguration $ describe "Explorer types" $ do
    describe "Bi instances" $ do
        binaryTest @TxExtra
