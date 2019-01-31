module Test.Pos.Launcher.Configuration (tests) where

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Universum

import           Test.Pos.Core.ExampleHelpers (feedPM)
import           Test.Pos.Launcher.Gen (genConfiguration)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonYamlShow)


roundTripConfiguration :: Property
roundTripConfiguration =
    roundTripsAesonYamlShow 1000 (feedPM genConfiguration)

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
