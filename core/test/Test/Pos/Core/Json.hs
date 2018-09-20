module Test.Pos.Core.Json where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Aeson.Core ()
import           Pos.Aeson.Core.Configuration ()
import           Pos.Aeson.Genesis ()

import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1, exampleAddress2,
                                               exampleAddress3, exampleAddress4,
                                               exampleGenesisConfiguration_GCSpec0,
                                               exampleGenesisConfiguration_GCSpec0YesNetworkMagic,
                                               exampleGenesisConfiguration_GCSpec1,
                                               exampleGenesisConfiguration_GCSpec2,
                                               exampleGenesisConfiguration_GCSrc,
                                               exampleGenesisData0, exampleGenesisData1,
                                               exampleGenesisData2,
                                               exampleGenesisProtocolConstants0,
                                               exampleGenesisProtocolConstants0YesNetworkMagic,
                                               exampleGenesisProtocolConstants1,
                                               exampleGenesisProtocolConstants2, feedPM)
import           Test.Pos.Core.Gen (genAddress, genBlockVersionData, genByte, genCoin,
                                    genCoinPortion, genEpochIndex, genFlatSlotId,
                                    genGenesisAvvmBalances, genGenesisConfiguration, genGenesisData,
                                    genGenesisDelegation, genGenesisInitializer,
                                    genGenesisProtocolConstants, genSharedSeed, genSoftforkRule,
                                    genTxFeePolicy)
import           Test.Pos.Crypto.Gen (genRedeemPublicKey)
import           Test.Pos.Util.Gen (genMillisecond)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestCanonicalJSONDec,
                                       goldenTestJSON, goldenTestJSONDec)
import           Test.Pos.Util.Tripping (discoverRoundTrip, roundTripsAesonBuildable,
                                         roundTripsAesonShow, roundTripsCanonicalJSONShow)

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------

golden_Address0 :: Property
golden_Address0 =
    goldenTestJSON
        exampleAddress
        "test/golden/json/Address0"

golden_Address1 :: Property
golden_Address1 =
    goldenTestJSON
        exampleAddress1
        "test/golden/json/Address1"

golden_Address2 :: Property
golden_Address2 =
    goldenTestJSON
        exampleAddress2
        "test/golden/json/Address2"

golden_Address3 :: Property
golden_Address3 =
    goldenTestJSON
        exampleAddress3
        "test/golden/json/Address3"

golden_Address4 :: Property
golden_Address4 =
    goldenTestJSON
        exampleAddress4
        "test/golden/json/Address4"

roundTripAddressShow :: Property
roundTripAddressShow =
    eachOf 100 genAddress roundTripsAesonShow

roundTripAddressBuildable :: Property
roundTripAddressBuildable =
    eachOf 100 genAddress roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- GenesisConfiguration
--------------------------------------------------------------------------------

golden_GenesisConfiguration_GCSpec0 :: Property
golden_GenesisConfiguration_GCSpec0 =
    goldenTestJSON
        exampleGenesisConfiguration_GCSpec0YesNetworkMagic
            "test/golden/json/GenesisConfiguration_GCSpec0_YesNetworkMagic"

-- Test only decoding (for ensuring backwards compatibility with
-- old GenesisConfiguration format).
golden_GenesisConfiguration_GCSpec0Dec :: Property
golden_GenesisConfiguration_GCSpec0Dec =
    goldenTestJSONDec exampleGenesisConfiguration_GCSpec0
        "test/golden/json/GenesisConfiguration_GCSpec0_NoNetworkMagic"

golden_GenesisConfiguration_GCSpec1Dec :: Property
golden_GenesisConfiguration_GCSpec1Dec =
    goldenTestJSONDec exampleGenesisConfiguration_GCSpec1
        "test/golden/json/GenesisConfiguration_GCSpec1_NoNetworkMagic"

golden_GenesisConfiguration_GCSpec2Dec :: Property
golden_GenesisConfiguration_GCSpec2Dec =
    goldenTestJSONDec exampleGenesisConfiguration_GCSpec2
        "test/golden/json/GenesisConfiguration_GCSpec2_NoNetworkMagic"

golden_GenesisConfiguration_GCSrc :: Property
golden_GenesisConfiguration_GCSrc =
    goldenTestJSON
        exampleGenesisConfiguration_GCSrc
            "test/golden/GenesisConfiguration_GCSrc"

roundTripGenesisConfiguration :: Property
roundTripGenesisConfiguration =
    eachOf 100 (feedPM genGenesisConfiguration) roundTripsAesonShow

--------------------------------------------------------------------------------
-- GenesisData (Canonical JSON)
--------------------------------------------------------------------------------

golden_GenesisDataDec0 :: Property
golden_GenesisDataDec0 =
    goldenTestCanonicalJSONDec
        exampleGenesisData0
            "test/golden/json/GenesisData0_NoNetworkMagic"

golden_GenesisDataDec1 :: Property
golden_GenesisDataDec1 =
    goldenTestCanonicalJSONDec
        exampleGenesisData1
            "test/golden/json/GenesisData1_NoNetworkMagic"

golden_GenesisDataDec2 :: Property
golden_GenesisDataDec2 =
    goldenTestCanonicalJSONDec
        exampleGenesisData2
            "test/golden/json/GenesisData2_NoNetworkMagic"

-- | This round-trip test no longer passes since `ProtocolMagic`'s `ToJSON`
-- instance no longer outputs the `RequireNetworkMagic` field's value.
_roundTripGenesisData :: Property
_roundTripGenesisData =
    eachOf 100 (feedPM genGenesisData) roundTripsCanonicalJSONShow

--------------------------------------------------------------------------------
-- GenesisAvvmBalances
--------------------------------------------------------------------------------

roundTripGenesisAvvmBalances :: Property
roundTripGenesisAvvmBalances =
     eachOf 100 genGenesisAvvmBalances roundTripsAesonShow

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

roundTripRedeemPublicKey :: Property
roundTripRedeemPublicKey = eachOf 1000 genRedeemPublicKey roundTripsAesonShow

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

roundTripCoin :: Property
roundTripCoin = eachOf 1000 genCoin roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- SharedSeed
--------------------------------------------------------------------------------

roundTripSharedSeed :: Property
roundTripSharedSeed = eachOf 1000 genSharedSeed roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- GenesisDelegation
--------------------------------------------------------------------------------

roundTripGenesisDelegation :: Property
roundTripGenesisDelegation =
    eachOf 100 (feedPM genGenesisDelegation) roundTripsAesonShow

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData =
    eachOf 1000 genBlockVersionData roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Millisecond
--------------------------------------------------------------------------------

roundTripMillisecond :: Property
roundTripMillisecond = eachOf 1000 genMillisecond roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- Byte
--------------------------------------------------------------------------------

roundTripByte :: Property
roundTripByte = eachOf 1000 genByte roundTripsAesonShow

--------------------------------------------------------------------------------
-- CoinPortion
--------------------------------------------------------------------------------

roundTripCoinPortion :: Property
roundTripCoinPortion = eachOf 1000 genCoinPortion roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------

roundTripFlatSlotId :: Property
roundTripFlatSlotId = eachOf 1000 genFlatSlotId roundTripsAesonShow

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 1000 genSoftforkRule roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------

roundTripTxFeePolicy :: Property
roundTripTxFeePolicy = eachOf 1000 genTxFeePolicy roundTripsAesonBuildable

--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------

roundTripEpochIndex :: Property
roundTripEpochIndex = eachOf 1000 genEpochIndex roundTripsAesonBuildable


--------------------------------------------------------------------------------
-- GenesisProtocolConstants
--------------------------------------------------------------------------------

golden_GenesisProtocolConstants0 :: Property
golden_GenesisProtocolConstants0 =
    goldenTestJSON
        exampleGenesisProtocolConstants0YesNetworkMagic
            "test/golden/json/GenesisProtocolConstants0_YesNetworkMagic"

-- Test only decoding (for ensuring backwards compatibility with
-- old GenesisProtocolConstants format).
golden_GenesisProtocolConstants0Dec :: Property
golden_GenesisProtocolConstants0Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants0
        "test/golden/json/GenesisProtocolConstants0_NoNetworkMagic"

golden_GenesisProtocolConstants1Dec :: Property
golden_GenesisProtocolConstants1Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants1
        "test/golden/json/GenesisProtocolConstants1_NoNetworkMagic"

golden_GenesisProtocolConstants2Dec :: Property
golden_GenesisProtocolConstants2Dec =
    goldenTestJSONDec exampleGenesisProtocolConstants2
        "test/golden/json/GenesisProtocolConstants2_NoNetworkMagic"

roundTripGenesisProtocolConstants :: Property
roundTripGenesisProtocolConstants =
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
