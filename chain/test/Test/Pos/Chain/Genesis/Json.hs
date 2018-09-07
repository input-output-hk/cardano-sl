{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Genesis.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Chain.Genesis.Example (exampleStaticConfig_GCSpec,
                     exampleStaticConfig_GCSrc)
import           Test.Pos.Chain.Genesis.Gen (genGenesisAvvmBalances,
                     genGenesisDelegation, genGenesisInitializer,
                     genGenesisProtocolConstants, genStaticConfig)
import           Test.Pos.Core.ExampleHelpers (feedPM)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow)

--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

golden_StaticConfig_GCSpec :: Property
golden_StaticConfig_GCSpec =
    goldenTestJSON
        exampleStaticConfig_GCSpec
            "test/golden/StaticConfig_GCSpec"

golden_StaticConfig_GCSrc :: Property
golden_StaticConfig_GCSrc =
    goldenTestJSON
        exampleStaticConfig_GCSrc
            "test/golden/StaticConfig_GCSrc"

roundTripStaticConfig :: Property
roundTripStaticConfig =
    eachOf 100 (feedPM genStaticConfig) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
     eachOf 100 genGenesisAvvmBalances roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
    eachOf 100 (feedPM genGenesisDelegation) roundTripsAesonShow

--------------------------------------------------------------------------------
-- ProtocolConstants
--------------------------------------------------------------------------------

roundTripProtocolConstants :: Property
roundTripProtocolConstants =
    eachOf 1000 genGenesisProtocolConstants roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
    eachOf 1000 genGenesisInitializer roundTripsAesonShow

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
