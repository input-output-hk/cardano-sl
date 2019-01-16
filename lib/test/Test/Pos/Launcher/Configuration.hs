module Test.Pos.Launcher.Configuration (tests) where

import           Hedgehog (Property)
import qualified Hedgehog as H
import           Universum

import           Test.Pos.Launcher.Gen (genConfiguration, genUpdate)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

roundTripConfiguration :: Property
roundTripConfiguration =
    eachOf 1000 genConfiguration roundTripsAesonShow

-- move roundTripUpdateConfiguration to chain project
roundTripUpdateConfiguration :: Property
roundTripUpdateConfiguration =
    eachOf 1000 genUpdate roundTripsAesonShow

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
