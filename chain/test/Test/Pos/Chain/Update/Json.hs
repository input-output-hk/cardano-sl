{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Update.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Chain.Update.Gen (genApplicationName,
                     genBlockVersionData, genSoftforkRule, genSoftwareVersion,
                     genSystemTag)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonBuildable)

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

roundTripApplicationName :: Property
roundTripApplicationName =
    eachOf 1000 genApplicationName roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion =
    eachOf 1000 genSoftwareVersion roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

roundTripSystemTag :: Property
roundTripSystemTag =
    eachOf 1000 genSystemTag roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Main Testing Function
--------------------------------------------------------------------------------

tests :: IO Bool
tests = H.checkParallel $$discoverRoundTrip
