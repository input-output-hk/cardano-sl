{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Genesis.Json
       ( tests
       ) where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Test.Pos.Chain.Genesis.Example (exampleGenesisData0,
                     exampleGenesisData1, exampleGenesisData2,
                     exampleGenesisProtocolConstants0,
                     exampleGenesisProtocolConstants1,
                     exampleGenesisProtocolConstants2,
                     exampleStaticConfig_GCSpec0, exampleStaticConfig_GCSpec1,
                     exampleStaticConfig_GCSpec2, exampleStaticConfig_GCSrc)
import           Test.Pos.Chain.Genesis.Gen (genGenesisAvvmBalances,
                     genGenesisData, genGenesisDelegation,
                     genGenesisInitializer, genGenesisProtocolConstants,
                     genStaticConfig)
import           Test.Pos.Core.ExampleHelpers (feedPM, feedPMWithRequiresMagic)
import           Test.Pos.Util.Golden (discoverGolden, eachOf,
                     goldenTestCanonicalJSONDec, goldenTestJSON,
                     goldenTestJSONDec)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonShow,
                     roundTripsCanonicalJSONShow)

--------------------------------------------------------------------------------
-- StaticConfig
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `StaticConfig` JSON format, the `RequiresNetworkMagic` field defaults to
-- `RequiresMagic`.

golden_StaticConfig_GCSpec0Dec :: Property
golden_StaticConfig_GCSpec0Dec =
    goldenTestJSONDec
        exampleStaticConfig_GCSpec0
            "test/golden/json/StaticConfig_GCSpec0_Legacy_HasNetworkMagic"

golden_StaticConfig_GCSpec1Dec :: Property
golden_StaticConfig_GCSpec1Dec =
    goldenTestJSONDec
        exampleStaticConfig_GCSpec1
            "test/golden/json/StaticConfig_GCSpec1_Legacy_HasNetworkMagic"

golden_StaticConfig_GCSpec2Dec :: Property
golden_StaticConfig_GCSpec2Dec =
    goldenTestJSONDec
        exampleStaticConfig_GCSpec2
            "test/golden/json/StaticConfig_GCSpec2_Legacy_HasNetworkMagic"

golden_StaticConfig_GCSrc :: Property
golden_StaticConfig_GCSrc =
    goldenTestJSON
        exampleStaticConfig_GCSrc
            "test/golden/json/StaticConfig_GCSrc"

roundTripStaticConfig :: Property
roundTripStaticConfig =
    eachOf 100 (feedPM genStaticConfig) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisData (Canonical JSON)
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `GenesisData` canonical JSON format, the `RequiresNetworkMagic` field
-- defaults to `RequiresMagic`.

golden_GenesisData0Dec :: Property
golden_GenesisData0Dec =
    goldenTestCanonicalJSONDec
        exampleGenesisData0
            "test/golden/canonical-json/GenesisData0_Legacy_HasNetworkMagic"

golden_GenesisDataDec1 :: Property
golden_GenesisDataDec1 =
    goldenTestCanonicalJSONDec
        exampleGenesisData1
            "test/golden/canonical-json/GenesisData1_Legacy_HasNetworkMagic"

golden_GenesisDataDec2 :: Property
golden_GenesisDataDec2 =
    goldenTestCanonicalJSONDec
        exampleGenesisData2
            "test/golden/canonical-json/GenesisData2_Legacy_HasNetworkMagic"

roundTripGenesisData :: Property
roundTripGenesisData =
    eachOf 100 (feedPMWithRequiresMagic genGenesisData) roundTripsCanonicalJSONShow

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
-- GenesisProtocolConstants
--------------------------------------------------------------------------------

-- Decode-only golden tests for ensuring that, when decoding the legacy
-- `GenesisProtocolConstants` JSON format, the `RequiresNetworkMagic` field
-- defaults to `RequiresMagic`.

golden_GenesisProtocolConstants0Dec :: Property
golden_GenesisProtocolConstants0Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants0
        "test/golden/json/GenesisProtocolConstants0_Legacy_HasNetworkMagic"

golden_GenesisProtocolConstants1Dec :: Property
golden_GenesisProtocolConstants1Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants1
        "test/golden/json/GenesisProtocolConstants1_Legacy_HasNetworkMagic"

golden_GenesisProtocolConstants2Dec :: Property
golden_GenesisProtocolConstants2Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants2
        "test/golden/json/GenesisProtocolConstants2_Legacy_HasNetworkMagic"

roundTripGenesisProtocolConstants :: Property
roundTripGenesisProtocolConstants =
    eachOf 1000 (feedPM genGenesisProtocolConstants) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisInitializer
--------------------------------------------------------------------------------

roundTripGenesisInitializer :: Property
roundTripGenesisInitializer =
    eachOf 1000 genGenesisInitializer roundTripsAesonShow

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
