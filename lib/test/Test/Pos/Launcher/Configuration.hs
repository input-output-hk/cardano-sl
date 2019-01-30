module Test.Pos.Launcher.Configuration (tests) where

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Universum

import           Test.Pos.Core.ExampleHelpers (feedPM)
import           Test.Pos.Launcher.Gen (genConfiguration)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)


roundTripConfiguration :: Property
roundTripConfiguration =
    eachOf 1000 (feedPM genConfiguration) roundTripsAesonShow

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
