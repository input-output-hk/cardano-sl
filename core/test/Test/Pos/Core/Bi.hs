{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Core.Bi
       ( tests
       ) where

import           Universum

import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import qualified Data.Map as M
import           Data.Time.Units (fromMicroseconds)
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Cardano.Crypto.Wallet (xpub)
import           Pos.Binary.Class (Raw (..))
import           Pos.Core.Block (BlockHeader (..), BlockHeaderAttributes, BlockSignature (..),
                                 GenesisBlockHeader, GenesisBody (..), GenesisConsensusData (..),
                                 GenesisProof (..), HeaderHash, MainBlockHeader, MainBody (..),
                                 MainConsensusData (..), MainExtraBodyData (..),
                                 MainExtraHeaderData (..), MainProof (..), MainToSign (..),
                                 mkGenesisHeader, mkMainHeaderExplicit)
import           Pos.Core.Common (AddrSpendingData (..), AddrStakeDistribution (..), AddrType (..),
                                  BlockCount (..), ChainDifficulty (..), Coeff (..), Coin (..),
                                  CoinPortion (..), Script (..), ScriptVersion, SharedSeed (..),
                                  StakeholderId, TxFeePolicy (..), TxSizeLinear (..))
import           Pos.Core.Configuration (GenesisHash (..))
import           Pos.Core.Delegation (DlgPayload (..), ProxySKBlockInfo)
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                    LocalSlotIndex (..), SlotCount (..), TimeDiff (..),
                                    Timestamp (..))
import           Pos.Core.Ssc (SscPayload (..), SscProof (..))
import           Pos.Core.Txp (Tx (..), TxInWitness (..), TxOutAux (..))
import           Pos.Core.Update (ApplicationName (..), SoftforkRule (..))
import           Pos.Crypto (Hash, ProtocolMagic (..), PublicKey (..), SignTag (..), abstractHash,
                             createPsk, hash, proxySign, redeemDeterministicKeyGen, sign, toPublic)
import           Pos.Data.Attributes (mkAttributes)
import           Pos.Merkle (mkMerkleTree, mtRoot)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi, roundTripsBiBuildable,
                                                          roundTripsBiShow)
import           Test.Pos.Core.ExampleHelpers (exampleAddrSpendingData_PubKey, exampleAddress,
                                               exampleAddress1, exampleAddress2, exampleAddress3,
                                               exampleAddress4, exampleBlockVersion,
                                               exampleBlockVersionData0,
                                               exampleBlockVersionModifier, exampleChainDifficulty,
                                               exampleCommitment, exampleCommitmentSignature,
                                               exampleCommitmentsMap, exampleEpochIndex,
                                               exampleHashTx, exampleInnerSharesMap,
                                               exampleLightDlgIndices, exampleOpening,
                                               exampleOpeningsMap, exampleProxySKBlockInfo,
                                               examplePublicKey, exampleRedeemPublicKey,
                                               exampleRedeemSignature, exampleScript,
                                               exampleSecretKey, exampleSecretKeys,
                                               exampleSharesDistribution, exampleSignedCommitment,
                                               exampleSlotId, exampleSlotLeaders,
                                               exampleSoftwareVersion, exampleSscPayload,
                                               exampleSscProof, exampleStakeholderId,
                                               exampleStakesList, exampleSystemTag, exampleTxId,
                                               exampleTxInList, exampleTxInUnknown, exampleTxInUtxo,
                                               exampleTxOut, exampleTxOutList, exampleTxPayload,
                                               exampleTxProof, exampleTxSig, exampleTxSigData,
                                               exampleTxWitness, exampleUpAttributes, exampleUpId,
                                               exampleUpdateData, exampleUpdatePayload,
                                               exampleUpdateProof, exampleUpdateProposal,
                                               exampleUpdateProposalToSign, exampleUpdateVote,
                                               exampleVoteId, exampleVssCertificate,
                                               exampleVssCertificatesHash,
                                               exampleVssCertificatesMap, feedPC, feedPM, feedPMC,
                                               staticHeavyDlgIndexes, staticProxySKHeavys)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- BlockBodyAttributes
--------------------------------------------------------------------------------
golden_BlockBodyAttributes :: Property
golden_BlockBodyAttributes = goldenTestBi bba "test/golden/BlockBodyAttributes"
  where
    bba = mkAttributes ()

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockHeader
--------------------------------------------------------------------------------
golden_BlockHeader_Genesis :: Property
golden_BlockHeader_Genesis =
    goldenTestBi exampleBlockHeaderGenesis "test/golden/BlockHeader_Genesis"

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
golden_BlockHeaderMain :: Property
golden_BlockHeaderMain =
    goldenTestBi exampleBlockHeaderMain "test/golden/BlockHeaderMain"

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi = eachOf 10 (feedPMC genBlockHeader) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockHeaderAttributes
--------------------------------------------------------------------------------
golden_BlockHeaderAttributes :: Property
golden_BlockHeaderAttributes = goldenTestBi (mkAttributes () :: BlockHeaderAttributes)
                                            "test/golden/BlockHeaderAttributes"

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockSignature
--------------------------------------------------------------------------------
golden_BlockSignature :: Property
golden_BlockSignature = goldenTestBi exampleBlockSignature "test/golden/BlockSignature"

golden_BlockSignature_Light :: Property
golden_BlockSignature_Light =
    goldenTestBi exampleBlockPSignatureLight "test/golden/BlockSignature_Light"

golden_BlockSignature_Heavy :: Property
golden_BlockSignature_Heavy =
    goldenTestBi exampleBlockPSignatureHeavy "test/golden/BlockSignature_Heavy"

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi = eachOf 10 (feedPMC genBlockSignature) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- GenesisBlockHeader
--------------------------------------------------------------------------------
golden_GenesisBlockHeader :: Property
golden_GenesisBlockHeader = goldenTestBi exampleGenesisBlockHeader
                                         "test/golden/GenesisBlockHeader"

roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi = eachOf 1000 (feedPM genGenesisBlockHeader) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- GenesisBody
--------------------------------------------------------------------------------
golden_GenesisBody :: Property
golden_GenesisBody = goldenTestBi exampleGenesisBody "test/golden/GenesisBody"

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow

--------------------------------------------------------------------------------
-- GenesisConsensusData
--------------------------------------------------------------------------------
golden_GenesisConsensusData :: Property
golden_GenesisConsensusData = goldenTestBi cd "test/golden/GenesisConsensusData"
  where cd = GenesisConsensusData exampleEpochIndex exampleChainDifficulty

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow

--------------------------------------------------------------------------------
-- HeaderHash
--------------------------------------------------------------------------------
golden_HeaderHash :: Property
golden_HeaderHash = goldenTestBi exampleHeaderHash "test/golden/HeaderHash"

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- GenesisProof
--------------------------------------------------------------------------------
golden_GenesisProof :: Property
golden_GenesisProof = goldenTestBi gp "test/golden/GenesisProof"
  where gp = GenesisProof (abstractHash exampleSlotLeaders)

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainBlockHeader
--------------------------------------------------------------------------------
golden_MainBlockHeader :: Property
golden_MainBlockHeader = goldenTestBi exampleMainBlockHeader "test/golden/MainBlockHeader"

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi = eachOf 20 (feedPMC genMainBlockHeader) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainBody
--------------------------------------------------------------------------------
golden_MainBody :: Property
golden_MainBody = goldenTestBi exampleMainBody "test/golden/MainBody"

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 20 (feedPM genMainBody) roundTripsBiShow

--------------------------------------------------------------------------------
-- MainConsensusData
--------------------------------------------------------------------------------
golden_MainConsensusData :: Property
golden_MainConsensusData = goldenTestBi mcd "test/golden/MainConsensusData"
  where mcd = MainConsensusData exampleSlotId examplePublicKey
                                exampleChainDifficulty exampleBlockSignature

roundTripMainConsensusData :: Property
roundTripMainConsensusData = eachOf 20 (feedPMC genMainConsensusData) roundTripsBiShow

--------------------------------------------------------------------------------
-- MainExtraBodyData
--------------------------------------------------------------------------------
golden_MainExtraBodyData :: Property
golden_MainExtraBodyData = goldenTestBi mebd "test/golden/MainExtraBodyData"
  where mebd = MainExtraBodyData (mkAttributes ())

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainExtraHeaderData
--------------------------------------------------------------------------------
golden_MainExtraHeaderData :: Property
golden_MainExtraHeaderData = goldenTestBi exampleMainExtraHeaderData
                                          "test/golden/MainExtraHeaderData"

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainProof
--------------------------------------------------------------------------------
golden_MainProof :: Property
golden_MainProof = goldenTestBi exampleMainProof "test/golden/MainProof"

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 20 (feedPM genMainProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- MainToSign
--------------------------------------------------------------------------------
golden_MainToSign :: Property
golden_MainToSign = goldenTestBi exampleMainToSign "test/golden/MainToSign"

roundTripMainToSignBi :: Property
roundTripMainToSignBi = eachOf 20 (feedPMC genMainToSign) roundTripsBiShow

--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
golden_Address :: Property
golden_Address = goldenTestBi exampleAddress "test/golden/bi/Address0"

golden_Address1 :: Property
golden_Address1 = goldenTestBi exampleAddress1 "test/golden/bi/Address1"

golden_Address2 :: Property
golden_Address2 = goldenTestBi exampleAddress2 "test/golden/bi/Address2"

golden_Address3 :: Property
golden_Address3 = goldenTestBi exampleAddress3 "test/golden/bi/Address3"

golden_Address4 :: Property
golden_Address4 = goldenTestBi exampleAddress4 "test/golden/bi/Address4"

roundTripAddressBi :: Property
roundTripAddressBi = eachOf 1000 genAddress roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrSpendingData
--------------------------------------------------------------------------------
golden_AddrSpendingData_PubKey :: Property
golden_AddrSpendingData_PubKey = goldenTestBi exampleAddrSpendingData_PubKey
                                              "test/golden/AddrSpendingData_PubKey"

golden_AddrSpendingData_Script :: Property
golden_AddrSpendingData_Script = goldenTestBi asd "test/golden/AddrSpendingData_Script"
  where asd = ScriptASD exampleScript

golden_AddrSpendingData_Redeem :: Property
golden_AddrSpendingData_Redeem = goldenTestBi asd "test/golden/AddrSpendingData_Redeem"
  where
    asd = RedeemASD redeemPublicKey
    Just redeemPublicKey = fst <$> redeemDeterministicKeyGen (getBytes 0 32)

golden_AddrSpendingData_Unknown :: Property
golden_AddrSpendingData_Unknown = goldenTestBi asd "test/golden/AddrSpendingData_Unknown"
  where asd = UnknownASD 247 (getBytes 3 32)

roundTripAddrSpendingDataBi :: Property
roundTripAddrSpendingDataBi = eachOf 1000 genAddrSpendingData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrStakeDistribution
--------------------------------------------------------------------------------
golden_AddrStakeDistribution_Bootstrap :: Property
golden_AddrStakeDistribution_Bootstrap =
    goldenTestBi BootstrapEraDistr "test/golden/AddrStakeDistribution_Bootstrap"

golden_AddrStakeDistribution_SingleKey :: Property
golden_AddrStakeDistribution_SingleKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_SingleKey"
  where
    asd = SingleKeyDistr (abstractHash examplePublicKey)

golden_AddrStakeDistribution_UnsafeMultiKey :: Property
golden_AddrStakeDistribution_UnsafeMultiKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_UnsafeMultiKey"
  where
    asd   =  M.fromList (zip sis coins) :: Map StakeholderId CoinPortion
    sis   = [si1, si2, si3]
    coins = map (CoinPortion . exp10_14) [3,2,5]
    exp10_14 x = x * (10 :: Word64) ^ (14 :: Word64)
    Right si1 = abstractHash . PublicKey <$> xpub (getBytes  0 64)
    Right si2 = abstractHash . PublicKey <$> xpub (getBytes 13 64)
    Right si3 = abstractHash . PublicKey <$> xpub (getBytes 27 64)

roundTripAddrStakeDistributionBi :: Property
roundTripAddrStakeDistributionBi = eachOf 1000 genAddrStakeDistribution roundTripsBiBuildable

--------------------------------------------------------------------------------
-- AddrType
--------------------------------------------------------------------------------
golden_AddrType_PK :: Property
golden_AddrType_PK = goldenTestBi ATPubKey "test/golden/AddrType_PK"

golden_AddrType_S :: Property
golden_AddrType_S = goldenTestBi ATScript "test/golden/AddrType_S"

golden_AddrType_R :: Property
golden_AddrType_R = goldenTestBi ATRedeem "test/golden/AddrType_R"

golden_AddrType_U :: Property
golden_AddrType_U = goldenTestBi (ATUnknown 57) "test/golden/AddrType_U"

roundTripAddrTypeBi :: Property
roundTripAddrTypeBi = eachOf 1000 genAddrType roundTripsBiShow

--------------------------------------------------------------------------------
-- BlockCount
--------------------------------------------------------------------------------
golden_BlockCount :: Property
golden_BlockCount = goldenTestBi bc "test/golden/BlockCount"
  where bc = BlockCount 999

roundTripBlockCountBi :: Property
roundTripBlockCountBi = eachOf 1000 genBlockCount roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ChainDifficulty
--------------------------------------------------------------------------------
golden_ChainDifficulty :: Property
golden_ChainDifficulty = goldenTestBi cd "test/golden/ChainDifficulty"
  where cd = ChainDifficulty (BlockCount 9999)

roundTripChainDifficultyBi :: Property
roundTripChainDifficultyBi = eachOf 1000 genChainDifficulty roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Coeff
--------------------------------------------------------------------------------
golden_Coeff :: Property
golden_Coeff = goldenTestBi c "test/golden/Coeff"
  where c = Coeff (MkFixed 101)

roundTripCoeffBi :: Property
roundTripCoeffBi = eachOf 1000 genCoeff roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Coin
--------------------------------------------------------------------------------
golden_Coin :: Property
golden_Coin = goldenTestBi c "test/golden/Coin"
  where c = Coin 9732

roundTripCoinBi :: Property
roundTripCoinBi = eachOf 1000 genCoin roundTripsBiBuildable

--------------------------------------------------------------------------------
-- CoinPortion
--------------------------------------------------------------------------------
golden_CoinPortion :: Property
golden_CoinPortion = goldenTestBi c "test/golden/CoinPortion"
  where c = CoinPortion 9702

roundTripCoinPortionBi :: Property
roundTripCoinPortionBi = eachOf 1000 genCoinPortion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Script
--------------------------------------------------------------------------------
golden_Script :: Property
golden_Script = goldenTestBi exampleScript "test/golden/Script"

roundTripScriptBi :: Property
roundTripScriptBi = eachOf 1000 genScript roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ScriptVersion
--------------------------------------------------------------------------------
golden_ScriptVersion :: Property
golden_ScriptVersion = goldenTestBi sv "test/golden/ScriptVersion"
  where sv = 6001 :: ScriptVersion

roundTripScriptVersionBi :: Property
roundTripScriptVersionBi = eachOf 1000 genScriptVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SharedSeed
--------------------------------------------------------------------------------
golden_SharedSeed :: Property
golden_SharedSeed = goldenTestBi s "test/golden/SharedSeed"
  where s = SharedSeed (getBytes 8 32)

roundTripSharedSeedBi :: Property
roundTripSharedSeedBi = eachOf 1000 genSharedSeed roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SlotLeaders
--------------------------------------------------------------------------------
golden_SlotLeaders :: Property
golden_SlotLeaders = goldenTestBi exampleSlotLeaders "test/golden/SlotLeaders"

roundTripSlotLeadersBi :: Property
roundTripSlotLeadersBi = eachOf 1000 genSlotLeaders roundTripsBiShow

--------------------------------------------------------------------------------
-- StakeholderId
--------------------------------------------------------------------------------
golden_StakeholderId :: Property
golden_StakeholderId =
    goldenTestBi exampleStakeholderId "test/golden/StakeholderId"

roundTripStakeholderIdBi :: Property
roundTripStakeholderIdBi = eachOf 1000 genStakeholderId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- StakesList
--------------------------------------------------------------------------------
golden_StakesList :: Property
golden_StakesList = goldenTestBi exampleStakesList "test/golden/StakesList"

roundTripStakesListBi :: Property
roundTripStakesListBi = eachOf 1000 genStakesList roundTripsBiShow

--------------------------------------------------------------------------------
-- StakesMap
--------------------------------------------------------------------------------
golden_StakesMap :: Property
golden_StakesMap = goldenTestBi sm "test/golden/StakesMap"
  where sm = HM.fromList exampleStakesList

roundTripStakesMapBi :: Property
roundTripStakesMapBi = eachOf 1000 genStakesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- TxFeePolicy
--------------------------------------------------------------------------------
golden_TxFeePolicy_Linear :: Property
golden_TxFeePolicy_Linear = goldenTestBi tfp "test/golden/TxFeePolicy_Linear"
  where
    tfp = TxFeePolicyTxSizeLinear (TxSizeLinear c1 c2)
    c1 = Coeff (MkFixed 99)
    c2 = Coeff (MkFixed 777)

golden_TxFeePolicy_Unknown :: Property
golden_TxFeePolicy_Unknown = goldenTestBi tfp "test/golden/TxFeePolicy_Unknown"
  where
    tfp = TxFeePolicyUnknown 101 (getBytes 40 32)

roundTripTxFeePolicyBi :: Property
roundTripTxFeePolicyBi = eachOf 1000 genTxFeePolicy roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSizeLinear
--------------------------------------------------------------------------------
golden_TxSizeLinear :: Property
golden_TxSizeLinear = goldenTestBi tsl "test/golden/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = Coeff (MkFixed 999)
    c2 = Coeff (MkFixed 77)

roundTripTxSizeLinearBi :: Property
roundTripTxSizeLinearBi = eachOf 1000 genTxSizeLinear roundTripsBiBuildable

--------------------------------------------------------------------------------
-- DlgPayload
--------------------------------------------------------------------------------
golden_DlgPayload :: Property
golden_DlgPayload = goldenTestBi dp "test/golden/DlgPayload"
  where dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genDlgPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HeavyDlgIndex
--------------------------------------------------------------------------------
golden_HeavyDlgIndex :: Property
golden_HeavyDlgIndex = goldenTestBi hdi "test/golden/HeavyDlgIndex"
  where hdi = staticHeavyDlgIndexes !! 0

roundTripHeavyDlgIndexBi :: Property
roundTripHeavyDlgIndexBi = eachOf 1000 genHeavyDlgIndex roundTripsBiBuildable

--------------------------------------------------------------------------------
-- LightDlgIndices
--------------------------------------------------------------------------------
golden_LightDlgIndices :: Property
golden_LightDlgIndices = goldenTestBi exampleLightDlgIndices
                                      "test/golden/LightDlgIndices"

roundTripLightDlgIndicesBi :: Property
roundTripLightDlgIndicesBi = eachOf 1000 genLightDlgIndices roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ProxySKBlockInfo
--------------------------------------------------------------------------------
golden_ProxySKBlockInfo_Nothing :: Property
golden_ProxySKBlockInfo_Nothing = goldenTestBi pskbi "test/golden/ProxySKBlockInfo_Nothing"
  where pskbi = Nothing :: ProxySKBlockInfo

golden_ProxySKBlockInfo_Just :: Property
golden_ProxySKBlockInfo_Just = goldenTestBi exampleProxySKBlockInfo
                                            "test/golden/ProxySKBlockInfo_Just"

roundTripProxySKBlockInfoBi :: Property
roundTripProxySKBlockInfoBi = eachOf 200 (feedPM genProxySKBlockInfo) roundTripsBiShow

--------------------------------------------------------------------------------
-- ProxySKHeavy
--------------------------------------------------------------------------------
golden_ProxySKHeavy :: Property
golden_ProxySKHeavy = goldenTestBi skh "test/golden/ProxySKHeavy"
  where skh = staticProxySKHeavys !! 0

roundTripProxySKHeavyBi :: Property
roundTripProxySKHeavyBi = eachOf 200 (feedPM genProxySKHeavy) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- EpochIndex
--------------------------------------------------------------------------------
golden_EpochIndex :: Property
golden_EpochIndex = goldenTestBi exampleEpochIndex "test/golden/EpochIndex"

roundTripEpochIndexBi :: Property
roundTripEpochIndexBi = eachOf 1000 genEpochIndex roundTripsBiBuildable

--------------------------------------------------------------------------------
-- EpochOrSlot
--------------------------------------------------------------------------------
golden_EpochOrSlotEI :: Property
golden_EpochOrSlotEI = goldenTestBi eos "test/golden/EpochOrSlotEI"
  where eos = EpochOrSlot (Left (EpochIndex 14))

golden_EpochOrSlotSI :: Property
golden_EpochOrSlotSI = goldenTestBi eos "test/golden/EpochOrSlotSI"
  where eos = EpochOrSlot (Right exampleSlotId)

roundTripEpochOrSlotBi :: Property
roundTripEpochOrSlotBi = eachOf 1000 (feedPC genEpochOrSlot) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- FlatSlotId
--------------------------------------------------------------------------------
golden_FlatSlotId :: Property
golden_FlatSlotId = goldenTestBi fsi "test/golden/FlatSlotId"
  where fsi = 5001 :: FlatSlotId

roundTripFlatSlotIdBi :: Property
roundTripFlatSlotIdBi = eachOf 1000 genFlatSlotId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- LocalSlotIndex
--------------------------------------------------------------------------------
golden_LocalSlotIndex :: Property
golden_LocalSlotIndex = goldenTestBi lsi "test/golden/LocalSlotIndex"
  where lsi = UnsafeLocalSlotIndex 52

roundTripLocalSlotIndexBi :: Property
roundTripLocalSlotIndexBi = eachOf 1000 (feedPC genLocalSlotIndex) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SlotCount
--------------------------------------------------------------------------------
golden_SlotCount :: Property
golden_SlotCount = goldenTestBi sc "test/golden/SlotCount"
  where sc = SlotCount 474747

roundTripSlotCountBi :: Property
roundTripSlotCountBi = eachOf 1000 genSlotCount roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SlotId
--------------------------------------------------------------------------------
golden_SlotId :: Property
golden_SlotId = goldenTestBi exampleSlotId "test/golden/SlotId"

roundTripSlotIdBi :: Property
roundTripSlotIdBi = eachOf 1000 (feedPC genSlotId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TimeDiff
--------------------------------------------------------------------------------
golden_TimeDiff :: Property
golden_TimeDiff = goldenTestBi td "test/golden/TimeDiff"
  where td = TimeDiff 4747

roundTripTimeDiffBi :: Property
roundTripTimeDiffBi = eachOf 1000 genTimeDiff roundTripsBiBuildable

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

golden_ApplicationName :: Property
golden_ApplicationName = goldenTestBi aN "test/golden/ApplicationName"
    where aN = ApplicationName "Golden"

roundTripApplicationName :: Property
roundTripApplicationName = eachOf 50 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

golden_Attributes :: Property
golden_Attributes = goldenTestBi attrib "test/golden/Attributes"
    where attrib = mkAttributes ()

roundTripAttributes :: Property
roundTripAttributes = eachOf 50 (genAttributes (pure ())) roundTripsBiShow

--------------------------------------------------------------------------------
-- BlockVersion
--------------------------------------------------------------------------------

golden_BlockVersion :: Property
golden_BlockVersion = goldenTestBi exampleBlockVersion "test/golden/BlockVersion"

roundTripBlockVersion :: Property
roundTripBlockVersion = eachOf 50 genBlockVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

golden_BlockVersionData :: Property
golden_BlockVersionData = goldenTestBi bVerDat "test/golden/BlockVersionData"
    where bVerDat = exampleBlockVersionData0

roundTripBlockVersionData :: Property
roundTripBlockVersionData = eachOf 50 genBlockVersionData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionModifier
--------------------------------------------------------------------------------

golden_BlockVersionModifier :: Property
golden_BlockVersionModifier = goldenTestBi bVerMod "test/golden/BlockVersionModifier"
    where bVerMod = exampleBlockVersionModifier

roundTripBlockVersionModifier :: Property
roundTripBlockVersionModifier = eachOf 50 genBlockVersionModifier roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

golden_Commitment :: Property
golden_Commitment = goldenTestBi exampleCommitment "test/golden/Commitment"

roundTripCommitment :: Property
roundTripCommitment = eachOf 10 genCommitment roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

golden_CommitmentsMap :: Property
golden_CommitmentsMap =
  goldenTestBi exampleCommitmentsMap "test/golden/CommitmentsMap"

roundTripCommitmentsMap :: Property
roundTripCommitmentsMap = eachOf 10 (feedPM genCommitmentsMap) roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsSignature
--------------------------------------------------------------------------------

golden_CommitmentSignature :: Property
golden_CommitmentSignature =
    goldenTestBi exampleCommitmentSignature "test/golden/CommitmentSignature"

roundTripCommitmentSignature :: Property
roundTripCommitmentSignature = eachOf 10 (feedPM genCommitmentSignature) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

golden_BlockHashRaw :: Property
golden_BlockHashRaw = goldenTestBi hRaw "test/golden/HashRaw"
    where hRaw = (abstractHash $ Raw ("9" ) :: Hash Raw)

roundTripHashRaw :: Property
roundTripHashRaw = eachOf 50 genHashRaw roundTripsBiBuildable

--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

golden_InnerSharesMap :: Property
golden_InnerSharesMap = goldenTestBi iSm "test/golden/InnerSharesMap"
    where iSm = exampleInnerSharesMap 3 1

roundTripInnerSharesMap :: Property
roundTripInnerSharesMap = eachOf 50 genInnerSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------

golden_MerkleTree :: Property
golden_MerkleTree = goldenTestBi mTree "test/golden/MerkleTree"
    where mTree = mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]


roundTripMerkleTree :: Property
roundTripMerkleTree = eachOf 10 (genMerkleTree genHashRaw) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------

golden_MerkleRoot :: Property
golden_MerkleRoot = goldenTestBi mTree "test/golden/MerkleRoot"
    where mTree = mtRoot $ mkMerkleTree [(abstractHash $ Raw ("9") :: Hash Raw)]

roundTripMerkleRoot :: Property
roundTripMerkleRoot = eachOf 10 (genMerkleRoot genHashRaw) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

golden_Opening :: Property
golden_Opening = goldenTestBi exampleOpening "test/golden/Opening"

roundTripOpening :: Property
roundTripOpening = eachOf 10 genOpening roundTripsBiBuildable

--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

golden_OpeningsMap :: Property
golden_OpeningsMap = goldenTestBi exampleOpeningsMap "test/golden/OpeningsMap"

roundTripOpeningsMap :: Property
roundTripOpeningsMap = eachOf 10 genOpeningsMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

golden_SignedCommitment :: Property
golden_SignedCommitment =
    goldenTestBi exampleSignedCommitment "test/golden/SignedCommitment"

roundTripSignedCommitment :: Property
roundTripSignedCommitment =
    eachOf 10 (feedPM genSignedCommitment) roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesDistribution
--------------------------------------------------------------------------------

golden_SharesDistribution :: Property
golden_SharesDistribution =
    goldenTestBi exampleSharesDistribution "test/golden/SharesDistribution"

roundTripSharesDistribution :: Property
roundTripSharesDistribution = eachOf 10 genSharesDistribution roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

golden_SharesMap :: Property
golden_SharesMap = goldenTestBi sM "test/golden/SharesMap"
    where
        sM = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

roundTripSharesMap :: Property
roundTripSharesMap = eachOf 10 genSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

golden_SoftforkRule :: Property
golden_SoftforkRule = goldenTestBi sfR "test/golden/SoftforkRule"
    where sfR = SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99)

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

golden_SoftwareVersion :: Property
golden_SoftwareVersion = goldenTestBi exampleSoftwareVersion "test/golden/SoftwareVersion"

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion = eachOf 10 genSoftwareVersion roundTripsBiBuildable


--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

golden_SscPayload_CommitmentsPayload :: Property
golden_SscPayload_CommitmentsPayload =
    goldenTestBi cP "test/golden/SscPayload_CommitmentsPayload"
  where
    cP = CommitmentsPayload exampleCommitmentsMap (exampleVssCertificatesMap 10 4)

golden_SscPayload_OpeningsPayload :: Property
golden_SscPayload_OpeningsPayload =
    goldenTestBi oP "test/golden/SscPayload_OpeningsPayload"
  where
    oP = OpeningsPayload exampleOpeningsMap (exampleVssCertificatesMap 10 4)


golden_SscPayload_SharesPayload :: Property
golden_SscPayload_SharesPayload =
    goldenTestBi exampleSscPayload "test/golden/SscPayload_SharesPayload"

golden_SscPayload_CertificatesPayload :: Property
golden_SscPayload_CertificatesPayload =
    goldenTestBi shP "test/golden/SscPayload_CertificatesPayload"
  where
    shP = CertificatesPayload (exampleVssCertificatesMap 10 4)


roundTripSscPayload :: Property
roundTripSscPayload = eachOf 10 (feedPM genSscPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

golden_SscProof_CommitmentsProof :: Property
golden_SscProof_CommitmentsProof =
    goldenTestBi exampleSscProof "test/golden/SscProof_CommitmentsProof"

golden_SscProof_OpeningsProof :: Property
golden_SscProof_OpeningsProof =
    goldenTestBi oP "test/golden/SscProof_OpeningsProof"
  where
    oP = OpeningsProof (hash exampleOpeningsMap) (exampleVssCertificatesHash 10 4)


golden_SscProof_SharesProof :: Property
golden_SscProof_SharesProof =
    goldenTestBi sP "test/golden/SscProof_SharesProof"
  where
    sP = SharesProof (hash exampleSharesMap) (exampleVssCertificatesHash 10 4)
    exampleSharesMap = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

golden_SscProof_CertificatesProof :: Property
golden_SscProof_CertificatesProof =
    goldenTestBi shP "test/golden/SscProof_CertificatesProof"
  where
    shP = CertificatesProof (exampleVssCertificatesHash 10 4)

roundTripSscProof :: Property
roundTripSscProof = eachOf 10 (feedPM genSscProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

golden_SystemTag :: Property
golden_SystemTag = goldenTestBi exampleSystemTag "test/golden/SystemTag"

roundTripSystemTag :: Property
roundTripSystemTag = eachOf 10 genSystemTag roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TimeStamp
--------------------------------------------------------------------------------

golden_Timestamp :: Property
golden_Timestamp = goldenTestBi timeStamp "test/golden/TimeStamp"
  where
    timeStamp = Timestamp $ fromMicroseconds 47

roundTripTimestamp :: Property
roundTripTimestamp = eachOf 50 genTimestamp roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

golden_Tx :: Property
golden_Tx = goldenTestBi tx "test/golden/Tx"
    where
        tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

roundTripTx :: Property
roundTripTx = eachOf 50 genTx roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

golden_TxAttributes :: Property
golden_TxAttributes = goldenTestBi txA "test/golden/TxAttributes"
    where
        txA = mkAttributes ()


roundTripTxAttributes :: Property
roundTripTxAttributes = eachOf 10 genTxAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAux
--------------------------------------------------------------------------------

roundTripTxAux :: Property
roundTripTxAux = eachOf 100 (feedPM genTxAux) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx Hash
--------------------------------------------------------------------------------

golden_HashTx :: Property
golden_HashTx = goldenTestBi exampleHashTx "test/golden/HashTx"

roundTripHashTx :: Property
roundTripHashTx = eachOf 50 genTxHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------


golden_TxInUtxo :: Property
golden_TxInUtxo = goldenTestBi exampleTxInUtxo "test/golden/TxIn_Utxo"

golden_TxInUnknown :: Property
golden_TxInUnknown = goldenTestBi exampleTxInUnknown "test/golden/TxIn_Unknown"

roundTripTxIn :: Property
roundTripTxIn = eachOf 100 genTxIn roundTripsBiBuildable


--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

golden_TxId :: Property
golden_TxId = goldenTestBi exampleTxId "test/golden/TxId"

roundTripTxId :: Property
roundTripTxId = eachOf 50 genTxId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxInList
--------------------------------------------------------------------------------

golden_TxInList :: Property
golden_TxInList = goldenTestBi exampleTxInList "test/golden/TxInList"

roundTripTxInList :: Property
roundTripTxInList = eachOf 50 genTxInList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxInWitness
--------------------------------------------------------------------------------

golden_PkWitness :: Property
golden_PkWitness = goldenTestBi pkWitness "test/golden/TxInWitness_PkWitness"
     where
        pkWitness = PkWitness examplePublicKey exampleTxSig

golden_ScriptWitness :: Property
golden_ScriptWitness = goldenTestBi scriptWitness "test/golden/TxInWitness_ScriptWitness"
    where
        scriptWitness = ScriptWitness validatorScript redeemerScript
        validatorScript = Script 47 "serialized script"
        redeemerScript = Script 47 "serialized script"


golden_RedeemWitness :: Property
golden_RedeemWitness = goldenTestBi redeemWitness "test/golden/TxInWitness_RedeemWitness"
    where
        redeemWitness = RedeemWitness exampleRedeemPublicKey exampleRedeemSignature

golden_UnknownWitnessType :: Property
golden_UnknownWitnessType = goldenTestBi unkWitType "test/golden/TxInWitness_UnknownWitnessType"
    where
        unkWitType = UnknownWitnessType 47 "forty seven"

-- 4000 because this should generate 1000 for each constructor

roundTripTxInWitness :: Property
roundTripTxInWitness = eachOf 50 (feedPM genTxInWitness) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutList
--------------------------------------------------------------------------------

golden_TxOutList :: Property
golden_TxOutList = goldenTestBi exampleTxOutList "test/golden/TxOutList"

roundTripTxOutList :: Property
roundTripTxOutList = eachOf 50 genTxOutList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

golden_TxOut :: Property
golden_TxOut = goldenTestBi exampleTxOut "test/golden/TxOut"

roundTripTxOut :: Property
roundTripTxOut = eachOf 50 genTxOut roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutAux
--------------------------------------------------------------------------------

golden_TxOutAux :: Property
golden_TxOutAux =  goldenTestBi txOutAux "test/golden/TxOutAux"
    where
        txOutAux = TxOutAux exampleTxOut

roundTripTxOutAux :: Property
roundTripTxOutAux = eachOf 50 genTxOutAux roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxPayload
--------------------------------------------------------------------------------

roundTripTxPayload :: Property
roundTripTxPayload = eachOf 50 (feedPM genTxPayload) roundTripsBiShow

--------------------------------------------------------------------------------
-- TxProof
--------------------------------------------------------------------------------

golden_TxProof :: Property
golden_TxProof =  goldenTestBi exampleTxProof "test/golden/TxProof"

roundTripTxProof :: Property
roundTripTxProof = eachOf 50 (feedPM genTxProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSig
--------------------------------------------------------------------------------

golden_TxSig :: Property
golden_TxSig = goldenTestBi txSigGold "test/golden/TxSig"
    where
        txSigGold = sign (ProtocolMagic 0) SignForTestingOnly
                         exampleSecretKey exampleTxSigData

roundTripTxSig :: Property
roundTripTxSig = eachOf 50 (feedPM genTxSig) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSigData
--------------------------------------------------------------------------------

golden_TxSigData :: Property
golden_TxSigData = goldenTestBi exampleTxSigData "test/golden/TxSigData"

roundTripTxSigData :: Property
roundTripTxSigData = eachOf 50 genTxSigData roundTripsBiShow

--------------------------------------------------------------------------------
-- TxWitness
--------------------------------------------------------------------------------

golden_TxWitness :: Property
golden_TxWitness = goldenTestBi exampleTxWitness "test/golden/TxWitness"

roundTripTxWitness :: Property
roundTripTxWitness = eachOf 20 (feedPM genTxWitness) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpAttributes
--------------------------------------------------------------------------------

golden_UpAttributes :: Property
golden_UpAttributes = goldenTestBi exampleUpAttributes "test/golden/UpAttributes"

roundTripUpAttributes :: Property
roundTripUpAttributes = eachOf 20 genUpAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

golden_UpdateData :: Property
golden_UpdateData = goldenTestBi exampleUpdateData "test/golden/UpdateData"

roundTripUpdateData :: Property
roundTripUpdateData = eachOf 20 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

golden_UpdatePayload :: Property
golden_UpdatePayload = goldenTestBi exampleUpdatePayload "test/golden/UpdatePayload"

roundTripUpdatePayload :: Property
roundTripUpdatePayload = eachOf 20 (feedPM genUpdatePayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

golden_UpdateProof :: Property
golden_UpdateProof = goldenTestBi exampleUpdateProof "test/golden/UpdateProof"

roundTripUpdateProof :: Property
roundTripUpdateProof = eachOf 20 (feedPM genUpdateProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

golden_UpdateProposal :: Property
golden_UpdateProposal = goldenTestBi exampleUpdateProposal "test/golden/UpdateProposal"

roundTripUpdateProposal :: Property
roundTripUpdateProposal = eachOf 20 (feedPM genUpdateProposal) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

golden_UpdateProposals :: Property
golden_UpdateProposals = goldenTestBi ups "test/golden/UpdateProposals"
  where
    -- Need to revisit this.
    ups = HM.fromList [(exampleUpId, exampleUpdateProposal)]

roundTripUpdateProposals :: Property
roundTripUpdateProposals = eachOf 20 (feedPM genUpdateProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateProposalToSign
--------------------------------------------------------------------------------

golden_UpdateProposalToSign :: Property
golden_UpdateProposalToSign =
  goldenTestBi exampleUpdateProposalToSign "test/golden/UpdateProposalToSign"

roundTripUpdateProposalToSign :: Property
roundTripUpdateProposalToSign = eachOf 20 genUpdateProposalToSign roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

golden_UpdateVote :: Property
golden_UpdateVote = goldenTestBi exampleUpdateVote "test/golden/UpdateVote"

roundTripUpdateVote :: Property
roundTripUpdateVote = eachOf 20 (feedPM genUpdateVote) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

golden_UpId :: Property
golden_UpId = goldenTestBi exampleUpId "test/golden/UpId"

roundTripUpId :: Property
roundTripUpId = eachOf 20 (feedPM genUpId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData NB: UpsData is not a type it is a record accessor of `UpdateProposalToSign`
--------------------------------------------------------------------------------

roundTripUpsData :: Property
roundTripUpsData = eachOf 20 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

golden_VoteId :: Property
golden_VoteId = goldenTestBi exampleVoteId "test/golden/VoteId"

roundTripVoteId :: Property
roundTripVoteId = eachOf 20 (feedPM genVoteId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

golden_VssCertificate :: Property
golden_VssCertificate = goldenTestBi exampleVssCertificate "test/golden/VssCertificate"

roundTripVssCertificate :: Property
roundTripVssCertificate = eachOf 10 (feedPM genVssCertificate) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

golden_VssCertificatesHash :: Property
golden_VssCertificatesHash = goldenTestBi (exampleVssCertificatesHash 10 4) "test/golden/VssCertificatesHash"

roundTripVssCertificatesHash :: Property
roundTripVssCertificatesHash = eachOf 10 (feedPM genVssCertificatesHash) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

golden_VssCertificatesMap :: Property
golden_VssCertificatesMap = goldenTestBi (exampleVssCertificatesMap 10 4) "test/golden/VssCertificatesMap"

roundTripVssCertificatesMap :: Property
roundTripVssCertificatesMap = eachOf 10 (feedPM genVssCertificatesMap) roundTripsBiShow

--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleBlockHeaderGenesis :: BlockHeader
exampleBlockHeaderGenesis = (BlockHeaderGenesis exampleGenesisBlockHeader)

exampleBlockHeaderMain :: MainBlockHeader
exampleBlockHeaderMain =
  mkMainHeaderExplicit (ProtocolMagic 0) exampleHeaderHash
                       exampleChainDifficulty exampleSlotId
                       exampleSecretKey Nothing
                       exampleMainBody exampleMainExtraHeaderData

exampleBlockSignature :: BlockSignature
exampleBlockSignature = BlockSignature (sign (ProtocolMagic 7)
                                              SignMainBlock
                                              exampleSecretKey
                                              exampleMainToSign)

exampleBlockPSignatureLight :: BlockSignature
exampleBlockPSignatureLight = BlockPSignatureLight sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk pm issuerSk (toPublic delegateSk) exampleLightDlgIndices
    pm = ProtocolMagic 2

exampleBlockPSignatureHeavy :: BlockSignature
exampleBlockPSignatureHeavy = BlockPSignatureHeavy sig
  where
    sig = proxySign pm SignProxySK delegateSk psk exampleMainToSign
    [delegateSk, issuerSk] = exampleSecretKeys 5 2
    psk = createPsk pm issuerSk (toPublic delegateSk) (staticHeavyDlgIndexes !! 0)
    pm = ProtocolMagic 2

exampleMainConsensusData :: MainConsensusData
exampleMainConsensusData = MainConsensusData exampleSlotId
                                             examplePublicKey
                                             exampleChainDifficulty
                                             exampleBlockSignature

exampleMainExtraHeaderData :: MainExtraHeaderData
exampleMainExtraHeaderData =
    MainExtraHeaderData exampleBlockVersion
                        exampleSoftwareVersion
                        (mkAttributes ())
                        (abstractHash (MainExtraBodyData (mkAttributes ())))

exampleGenesisBlockHeader :: GenesisBlockHeader
exampleGenesisBlockHeader = mkGenesisHeader (ProtocolMagic 0)
                                            (Left (GenesisHash prevHash))
                                            (EpochIndex 11)
                                            exampleGenesisBody
  where
    prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a

-- We use `Nothing` as the ProxySKBlockInfo to avoid clashing key errors
-- (since we use example keys which aren't related to each other)
exampleMainBlockHeader :: MainBlockHeader
exampleMainBlockHeader = mkMainHeaderExplicit (ProtocolMagic 7)
                                              exampleHeaderHash
                                              exampleChainDifficulty
                                              exampleSlotId
                                              exampleSecretKey
                                              Nothing
                                              exampleMainBody
                                              exampleMainExtraHeaderData

exampleMainProof :: MainProof
exampleMainProof = MainProof exampleTxProof exampleSscProof
                             (abstractHash dp) exampleUpdateProof
  where
    dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

exampleHeaderHash :: HeaderHash
exampleHeaderHash = coerce (hash ("HeaderHash" :: Text))

exampleGenesisBody :: GenesisBody
exampleGenesisBody = GenesisBody exampleSlotLeaders

exampleMainBody :: MainBody
exampleMainBody = MainBody exampleTxPayload exampleSscPayload
                           dp exampleUpdatePayload
  where
    dp = UnsafeDlgPayload (take 4 staticProxySKHeavys)

exampleMainToSign :: MainToSign
exampleMainToSign = MainToSign (abstractHash (BlockHeaderGenesis exampleGenesisBlockHeader))
                    exampleMainProof exampleSlotId exampleChainDifficulty exampleMainExtraHeaderData

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
     , H.checkParallel $$discoverRoundTrip
    ]
