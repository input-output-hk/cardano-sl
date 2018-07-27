{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Infra.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Test.Pos.Infra.Gen (genDnsDomains, genDomain, genMaxBucketSize,
                     genNodeAddr, genNodeAddrMaybe, genNodeMetaData,
                     genNodeName, genNodeRegion, genNodeRoutes, genNodeType)
import           Test.Pos.Util.Golden (eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)



--------------------------------------------------------------------------------
-- NodeMetaData
--------------------------------------------------------------------------------

roundTripNodeMetaData :: Property
roundTripNodeMetaData =
    eachOf 1000 genNodeMetaData roundTripsAesonShow

--------------------------------------------------------------------------------
-- NodeName
--------------------------------------------------------------------------------

roundTripNodeName :: Property
roundTripNodeName =
    eachOf 1000 genNodeName roundTripsAesonShow

--------------------------------------------------------------------------------
-- NodeRegion
--------------------------------------------------------------------------------

roundTripNodeRegion :: Property
roundTripNodeRegion =
    eachOf 1000 genNodeRegion roundTripsAesonShow

--------------------------------------------------------------------------------
-- NodeRoutes
--------------------------------------------------------------------------------

roundTripNodeRoutes :: Property
roundTripNodeRoutes =
    eachOf 1000 genNodeRoutes roundTripsAesonShow

--------------------------------------------------------------------------------
-- NodeType
--------------------------------------------------------------------------------

roundTripNodeType :: Property
roundTripNodeType =
    eachOf 1000 genNodeType roundTripsAesonShow

--------------------------------------------------------------------------------
-- DnsDomains
--------------------------------------------------------------------------------

roundTripDnsDomains :: Property
roundTripDnsDomains =
    eachOf 1000 genDnsDomains roundTripsAesonShow

--------------------------------------------------------------------------------
-- Valency
--------------------------------------------------------------------------------

roundTripValency :: Property
roundTripValency =
    eachOf 1000 (Gen.int (Range.constant 1 100)) roundTripsAesonShow

--------------------------------------------------------------------------------
-- Fallbacks
--------------------------------------------------------------------------------

roundTripFallbacks :: Property
roundTripFallbacks =
    eachOf 1000 (Gen.int (Range.constant 1 100)) roundTripsAesonShow

--------------------------------------------------------------------------------
-- NodeAddr (Maybe DNS.Domain)
--------------------------------------------------------------------------------

roundTripNodeAddrMaybe :: Property
roundTripNodeAddrMaybe =
    eachOf 1000 genNodeAddrMaybe roundTripsAesonShow

--------------------------------------------------------------------------------
-- NodeAddr
--------------------------------------------------------------------------------

roundTripNodeAddr :: Property
roundTripNodeAddr =
    eachOf 1000 (genNodeAddr genDomain) roundTripsAesonShow

--------------------------------------------------------------------------------
-- MaxBucketSize
--------------------------------------------------------------------------------

roundTripMaxBucketSize :: Property
roundTripMaxBucketSize =
    eachOf 100 genMaxBucketSize roundTripsAesonShow

tests :: IO Bool
tests =  H.checkParallel $$discoverRoundTrip

