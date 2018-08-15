{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Core.Bi
       ( tests
       ) where

import           Universum

import           Crypto.Hash (Blake2b_224)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List ((!!))
import qualified Data.Map as M
import           Data.Time.Units (fromMicroseconds)
import           Data.Typeable (typeRep)
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

import           Cardano.Crypto.Wallet (xpub)
import           Pos.Binary.Class (Bi, Case (..), Raw (..), SizeOverride (..),
                     szCases)
import           Pos.Core.Attributes (Attributes, mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), AddrType (..),
                     BlockCount (..), ChainDifficulty (..), Coeff (..),
                     Coin (..), CoinPortion (..), ScriptVersion,
                     SharedSeed (..), StakeholderId, TxFeePolicy (..),
                     TxSizeLinear (..))
import           Pos.Core.Delegation (DlgPayload (..), ProxySKBlockInfo)
import           Pos.Core.Merkle (mkMerkleTree, mtRoot)
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                     FlatSlotId, LocalSlotIndex (..), SlotCount (..),
                     TimeDiff (..), Timestamp (..))
import           Pos.Core.Ssc (SscPayload (..), SscProof (..))
import           Pos.Core.Update (ApplicationName (..), SoftforkRule (..))
import           Pos.Crypto (AbstractHash (..), Hash, PublicKey (..),
                     abstractHash, hash, redeemDeterministicKeyGen)


import           Test.Pos.Binary.Helpers (SizeTestConfig (..), scfg, sizeTest)
import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Core.ExampleHelpers (exampleAddrSpendingData_PubKey,
                     exampleAddress, exampleAddress1, exampleAddress2,
                     exampleAddress3, exampleAddress4, exampleBlockVersion,
                     exampleBlockVersionData, exampleBlockVersionModifier,
                     exampleCommitment, exampleCommitmentSignature,
                     exampleCommitmentsMap, exampleEpochIndex,
                     exampleInnerSharesMap, exampleLightDlgIndices,
                     exampleOpening, exampleOpeningsMap,
                     exampleProxySKBlockInfo, examplePublicKey, exampleScript,
                     exampleSharesDistribution, exampleSignedCommitment,
                     exampleSlotId, exampleSlotLeaders, exampleSoftwareVersion,
                     exampleSscPayload, exampleSscProof, exampleStakeholderId,
                     exampleStakesList, exampleSystemTag, exampleUpAttributes,
                     exampleUpId, exampleUpdateData, exampleUpdatePayload,
                     exampleUpdateProof, exampleUpdateProposal,
                     exampleUpdateProposalToSign, exampleUpdateVote,
                     exampleVoteId, exampleVssCertificate,
                     exampleVssCertificatesHash, exampleVssCertificatesMap,
                     feedPC, feedPM, staticHeavyDlgIndexes,
                     staticProxySKHeavys)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)



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
    where bVerDat = exampleBlockVersionData

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

sizeEstimates :: H.Group
sizeEstimates =
  let check :: forall a. (Show a, Bi a) => Gen a -> Property
      check g = sizeTest $ scfg { gen = g }
      pkOrRedeem (PubKeyASD _) = True
      pkOrRedeem (RedeemASD _) = True
      pkOrRedeem _             = False

      -- Explicit bounds for types, based on the generators from Gen.
      attrUnitSize = (typeRep (Proxy @(Attributes ()))
                     , SizeConstant 1)
      attrAddrSize = (typeRep (Proxy @(Attributes AddrAttributes)),
                      SizeConstant (szCases [ Case "min" 1, Case "max" 1024 ]))
      portionSize  = (typeRep (Proxy @(Map (AbstractHash Blake2b_224 PublicKey) CoinPortion)),
                      SizeConstant (szCases [ Case "min" 1, Case "max" 1024 ]))

  in H.Group "Encoded size bounds for core types."
        [ ("Coin"                 , check genCoin)
        , ("BlockCount"           , check genBlockCount)
        , ("Attributes ()"        , sizeTest $ scfg
              { gen = genAttributes (pure ())
              , addlCtx = M.fromList [ attrUnitSize ]
              })
        , ("Attributes AddrAttributes", sizeTest $ scfg
              { gen = genAttributes genAddrAttributes
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("Address"              , sizeTest $ scfg
              { gen = genAddress
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("AddrStakeDistribution", sizeTest $ scfg
              { gen = genAddrStakeDistribution
              , addlCtx = M.fromList [ portionSize ]
              })
        , ("AddrSpendingData"     , sizeTest $ scfg
              { gen = Gen.filter pkOrRedeem genAddrSpendingData
              , addlCtx = M.fromList
                  [ (typeRep (Proxy @AddrSpendingData),
                     SelectCases ["PubKeyASD", "RedeemASD"])
                  ] })
        , ("AddrType"             , check genAddrType)
        ]

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    , H.checkParallel sizeEstimates
    ]
