module Test.Pos.Core.Bi
    -- ( tests
    -- , roundTripAddressBi
    -- ) where
    where

import           Universum

import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (discoverGolden, discoverRoundTrip, eachOf,
                                                          -- goldenTestBi, roundTripsAesonBuildable,
                                                          -- roundTripsAesonShow,
                                                          roundTripsBiBuildable,
                                                          roundTripsBiShow)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Gen (genProtocolMagic)


--------------------------------------------------------------------------------
-- Pos.Core.Block
--------------------------------------------------------------------------------

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable

-- fails
-- recheck (Size 0) (Seed 12053450548120184651 7567781078738770419) roundTripBlockHeaderBi
roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi = eachOf 10 genBlockHeader roundTripsBiBuildable

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable

-- slow
-- fails
-- recheck (Size 1) (Seed 8796295392233115961 17003651516827452701) roundTripBlockSignatureBi
roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi = eachOf 1000 (feedPM genBlockSignature) roundTripsBiBuildable

-- fails
roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi = eachOf 1000 (feedPM genGenesisBlockHeader) roundTripsBiBuildable

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow

-- GenesisHash is just a newtype around a Hash, and lacks Bi instances. The newtype is
-- unwrapped when constructing a block, so it doesn't appear anywhere and we don't need
-- to test it.

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable

-- slow
-- fails
-- recheck (Size 0) (Seed 2267974326261439600 3208838063806141983) roundTripMainBlockHeaderBi
roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi = eachOf 1000 genMainBlockHeader roundTripsBiBuildable

-- fails
-- recheck (Size 0) (Seed 7561563662088777279 876367418530652631) roundTripMainBodyBi
roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 10 (feedPM genMainBody) roundTripsBiShow

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable

-- slow
roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 10 genMainProof roundTripsBiBuildable

-- fails
roundTripMainToSignBi :: Property
roundTripMainToSignBi = eachOf 1000 genMainToSign roundTripsBiShow


-- group 1

-- TODO mhueschen grok why this doesn't have a Bi instance, but (Attributes AddrAttributes) does
-- ^ see module Pos.Core.Common.AddrAttributes
-- roundTripAddrAttributesBi :: Property
-- roundTripAddrAttributesBi = eachOf 1000 genAddrAttributes roundTripsBiBuildable

roundTripAddressBi :: Property
roundTripAddressBi = eachOf 1000 genAddress roundTripsBiBuildable

roundTripAddrSpendingDataBi :: Property
roundTripAddrSpendingDataBi = eachOf 1000 genAddrSpendingData roundTripsBiBuildable

roundTripAddrStakeDistributionBi :: Property
roundTripAddrStakeDistributionBi = eachOf 1000 genAddrStakeDistribution roundTripsBiBuildable

roundTripAddrTypeBi :: Property
roundTripAddrTypeBi = eachOf 1000 genAddrType roundTripsBiShow

roundTripBlockCountBi :: Property
roundTripBlockCountBi = eachOf 1000 genBlockCount roundTripsBiBuildable

roundTripChainDifficultyBi :: Property
roundTripChainDifficultyBi = eachOf 1000 genChainDifficulty roundTripsBiBuildable

roundTripCoeffBi :: Property
roundTripCoeffBi = eachOf 1000 genCoeff roundTripsBiBuildable

roundTripCoinBi :: Property
roundTripCoinBi = eachOf 1000 genCoin roundTripsBiBuildable

roundTripCoinPortionBi :: Property
roundTripCoinPortionBi = eachOf 1000 genCoinPortion roundTripsBiBuildable

roundTripScriptBi :: Property
roundTripScriptBi = eachOf 1000 genScript roundTripsBiBuildable

roundTripScriptVersionBi :: Property
roundTripScriptVersionBi = eachOf 1000 genScriptVersion roundTripsBiBuildable

roundTripSharedSeedBi :: Property
roundTripSharedSeedBi = eachOf 1000 genSharedSeed roundTripsBiBuildable

roundTripSlotLeadersBi :: Property
roundTripSlotLeadersBi = eachOf 1000 genSlotLeaders roundTripsBiShow

roundTripStakeholderIdBi :: Property
roundTripStakeholderIdBi = eachOf 1000 genStakeholderId roundTripsBiBuildable

roundTripStakesListBi :: Property
roundTripStakesListBi = eachOf 1000 genStakesList roundTripsBiShow

roundTripStakesMapBi :: Property
roundTripStakesMapBi = eachOf 1000 genStakesMap roundTripsBiShow

roundTripTxFeePolicyBi :: Property
roundTripTxFeePolicyBi = eachOf 1000 genTxFeePolicy roundTripsBiBuildable

roundTripTxSizeLinearBi :: Property
roundTripTxSizeLinearBi = eachOf 1000 genTxSizeLinear roundTripsBiBuildable

-- group 2

-- no Bi instance
-- roundTripGenesisConfigurationBi :: Property
-- roundTripGenesisConfigurationBi = eachOf 1000 genGenesisConfiguration roundTripsBiBuildable

-- no Bi instance
-- roundTripCoreConfigurationBi :: Property
-- roundTripCoreConfigurationBi = eachOf 1000 genCoreConfiguration roundTripsBiBuildable

-- slow!!! shrink gens or # tests?
roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genDlgPayload) roundTripsBiBuildable

roundTripHeavyDlgIndexBi :: Property
roundTripHeavyDlgIndexBi = eachOf 1000 genHeavyDlgIndex roundTripsBiBuildable

roundTripLightDlgIndicesBi :: Property
roundTripLightDlgIndicesBi = eachOf 1000 genLightDlgIndices roundTripsBiBuildable

-- also kinda slow
roundTripProxySKBlockInfoBi :: Property
roundTripProxySKBlockInfoBi = eachOf 500 genProxySKBlockInfo roundTripsBiShow

-- also kinda slow
roundTripProxySKHeavyBi :: Property
roundTripProxySKHeavyBi = eachOf 500 (feedPM genProxySKHeavy) roundTripsBiBuildable

-- no Bi instance
-- roundTripFakeAvvmOptionsBi :: Property
-- roundTripFakeAvvmOptionsBi = eachOf 1000 genFakeAvvmOptions roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisAvvmBalancesBi :: Property
-- roundTripGenesisAvvmBalancesBi = eachOf 1000 genGenesisAvvmBalances roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisDelegationBi :: Property
-- roundTripGenesisDelegationBi = eachOf 1000 genGenesisDelegation roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisProtocolConstantsBi :: Property
-- roundTripGenesisProtocolConstantsBi = eachOf 1000 genGenesisProtocolConstants roundTripsBiBuildable

-- no Bi instance
-- roundTripGenesisSpecBi :: Property
-- roundTripGenesisSpecBi = eachOf 1000 genGenesisSpec roundTripsBiBuildable

-- no Bi instance
-- roundTripTestnetBalanceOptionsBi :: Property
-- roundTripTestnetBalanceOptionsBi = eachOf 1000 genTestnetBalanceOptions roundTripsBiBuildable

-- no Bi instance
-- roundTripVssMaxTTLBi :: Property
-- roundTripVssMaxTTLBi = eachOf 1000 genVssMaxTTL roundTripsBiBuildable

-- no Bi instance
-- roundTripVssMinTTLBi :: Property
-- roundTripVssMinTTLBi = eachOf 1000 genVssMinTTL roundTripsBiBuildable

roundTripEpochIndexBi :: Property
roundTripEpochIndexBi = eachOf 1000 genEpochIndex roundTripsBiBuildable

roundTripEpochOrSlotBi :: Property
roundTripEpochOrSlotBi = eachOf 1000 genEpochOrSlot roundTripsBiBuildable

roundTripFlatSlotIdBi :: Property
roundTripFlatSlotIdBi = eachOf 1000 genFlatSlotId roundTripsBiBuildable

roundTripLocalSlotIndexBi :: Property
roundTripLocalSlotIndexBi = eachOf 1000 genLocalSlotIndex roundTripsBiBuildable

roundTripSlotCountBi :: Property
roundTripSlotCountBi = eachOf 1000 genSlotCount roundTripsBiBuildable

roundTripSlotIdBi :: Property
roundTripSlotIdBi = eachOf 1000 genSlotId roundTripsBiBuildable

roundTripTimeDiffBi :: Property
roundTripTimeDiffBi = eachOf 1000 genTimeDiff roundTripsBiBuildable


-- Jordan's types

-- slow
roundTripSscPayloadBi :: Property
roundTripSscPayloadBi = eachOf 20 (feedPM genSscPayload) roundTripsBiBuildable

roundTripUpdatePayloadBi :: Property
roundTripUpdatePayloadBi = eachOf 100 (feedPM genUpdatePayload) roundTripsBiBuildable

-- fails
roundTripTxPayloadBi :: Property
roundTripTxPayloadBi = eachOf 100 (feedPM genTxPayload) roundTripsBiShow

-- fails
roundTripTxAuxBi :: Property
roundTripTxAuxBi = eachOf 1000 (feedPM genTxAux) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- TODO move this into util package? or crypto.gen?
feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkSequential $$discoverRoundTrip
