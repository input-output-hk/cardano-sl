-- | This module tests Binary instances.

module Test.Pos.DHT.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec (Spec, describe)
import           Universum

import           Pos.Arbitrary.Infra ()
import qualified Pos.DHT.Model as DHT

import           Test.Pos.Helpers (binaryTest)

spec :: Spec
spec = describe "DHT.Model" $ do
    describe "Bi instances" $ do
        binaryTest @DHT.DHTKey
        binaryTest @DHT.DHTData
