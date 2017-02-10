-- | This module tests Binary instances.

module Test.Pos.Genesis.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec            (Spec, describe)
import           Universum

import qualified Pos.Genesis           as T
import           Pos.Genesis.Arbitrary ()

import           Test.Pos.Util         (binaryTest)

spec :: Spec
spec = describe "Genesis" $ do
    describe "Bi instances" $ do
        binaryTest @T.StakeDistribution
        binaryTest @T.GenesisData
