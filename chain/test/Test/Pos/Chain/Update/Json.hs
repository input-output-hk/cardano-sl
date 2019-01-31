{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Update.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Chain.Update.Gen (genApplicationName,
                     genBlockVersionData, genSoftforkRule, genSoftwareVersion,
                     genSystemTag, genUpdateConfiguration)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonYamlBuildable, roundTripsAesonYamlShow)


--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

roundTripApplicationName :: Property
roundTripApplicationName =
    eachOf 1000 genApplicationName roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion =
    eachOf 1000 genSoftwareVersion roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

roundTripSystemTag :: Property
roundTripSystemTag =
    eachOf 1000 genSystemTag roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- UpdateConfiguration
--------------------------------------------------------------------------------

roundTripUpdateConfiguration :: Property
roundTripUpdateConfiguration =
    roundTripsAesonYamlShow 1000 genUpdateConfiguration

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
