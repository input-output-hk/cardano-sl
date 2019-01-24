module Test.Pos.Launcher.Configuration (tests, roundTripConfiguration) where

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Universum

import           Test.Pos.Core.ExampleHelpers (feedEpochSlots)
import           Test.Pos.Launcher.Gen (genConfiguration, genUpdate)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

roundTripConfiguration :: Property
roundTripConfiguration =
    eachOf 1000 (feedEpochSlots genConfiguration) roundTripsAesonShow

-- move roundTripUpdateConfiguration to chain project
roundTripUpdateConfiguration :: Property
roundTripUpdateConfiguration =
    eachOf 1000 genUpdate roundTripsAesonShow

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
