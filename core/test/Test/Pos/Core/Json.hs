module Test.Pos.Core.Json where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Core.JsonLog.LogEvents (InvReqDataFlowLog (..))

import           Test.Pos.Core.ExampleHelpers (exampleAddress, exampleAddress1,
                     exampleAddress2, exampleAddress3, exampleAddress4,
                     feedEpochSlots)
import           Test.Pos.Core.Gen (genAddress, genByte, genCoin,
                     genCoinPortion, genEpochIndex, genEpochOrSlot,
                     genFlatSlotId, genInvReqDataFlowLog, genSharedSeed,
                     genTxFeePolicy)
import           Test.Pos.Crypto.Gen (genRedeemPublicKey)
import           Test.Pos.Util.Gen (genMillisecond)
import           Test.Pos.Util.Golden (discoverGolden, eachOf, goldenTestJSON)
import           Test.Pos.Util.Tripping (discoverRoundTrip,
                     roundTripsAesonYamlBuildable, roundTripsAesonYamlShow)

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
    roundTripsAesonYamlShow 100 genAddress

roundTripAddressBuildable :: Property
roundTripAddressBuildable =
    eachOf 100 genAddress roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- RedeemPublicKey
--------------------------------------------------------------------------------

roundTripRedeemPublicKey :: Property
roundTripRedeemPublicKey = roundTripsAesonYamlShow 1000 genRedeemPublicKey

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------

roundTripCoin :: Property
roundTripCoin = eachOf 1000 genCoin roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- SharedSeed
--------------------------------------------------------------------------------

roundTripSharedSeed :: Property
roundTripSharedSeed = eachOf 1000 genSharedSeed roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- Millisecond
--------------------------------------------------------------------------------

roundTripMillisecond :: Property
roundTripMillisecond = eachOf 1000 genMillisecond roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- Byte
--------------------------------------------------------------------------------

roundTripByte :: Property
roundTripByte = roundTripsAesonYamlShow 1000 genByte

--------------------------------------------------------------------------------
-- CoinPortion
--------------------------------------------------------------------------------

roundTripCoinPortion :: Property
roundTripCoinPortion = eachOf 1000 genCoinPortion roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------

roundTripFlatSlotId :: Property
roundTripFlatSlotId = roundTripsAesonYamlShow 1000 genFlatSlotId

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------

roundTripTxFeePolicy :: Property
roundTripTxFeePolicy = eachOf 1000 genTxFeePolicy roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------

roundTripEpochIndex :: Property
roundTripEpochIndex = eachOf 1000 genEpochIndex roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- EpochOrSlot
--------------------------------------------------------------------------------

roundTripEpochOrSlot :: Property
roundTripEpochOrSlot = eachOf 1000 (feedEpochSlots genEpochOrSlot) roundTripsAesonYamlBuildable

--------------------------------------------------------------------------------
-- InvReqDataFlowLog
--------------------------------------------------------------------------------

golden_InvReqDataFlowLog_InvReqAccepted :: Property
golden_InvReqDataFlowLog_InvReqAccepted =
    goldenTestJSON
        (InvReqAccepted 1 2 3 4)
            "test/golden/InvReqDataFlowLog_InvReqAccepted"

golden_InvReqDataFlowLog_InvReqRejected :: Property
golden_InvReqDataFlowLog_InvReqRejected =
    goldenTestJSON
        (InvReqRejected 1 2)
            "test/golden/InvReqDataFlowLog_InvReqRejected"

golden_InvReqDataFlowLog_InvReqException :: Property
golden_InvReqDataFlowLog_InvReqException =
    goldenTestJSON
        (InvReqException "test")
            "test/golden/InvReqDataFlowLog_InvReqException"

roundTripInvReqDataFlowLog :: Property
roundTripInvReqDataFlowLog =
    roundTripsAesonYamlShow 1000 genInvReqDataFlowLog

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkParallel $$discoverRoundTrip
