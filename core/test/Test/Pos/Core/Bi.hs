{-# LANGUAGE OverloadedStrings #-}
module Test.Pos.Core.Bi
       ( tests

       -- Example data
       , exampleBlockVersion
       , exampleChainDifficulty
       , exampleEpochIndex
       , exampleLightDlgIndices
       , examplePublicKey
       , exampleSecretKey
       , exampleSecretKeys
       , exampleSlotId
       , exampleSlotLeaders
       , exampleSoftwareVersion
       , exampleSscPayload
       , exampleSscProof
       , exampleTxPayload
       , exampleTxProof
       , exampleUpdatePayload
       , exampleUpdateProof
       , staticHeavyDlgIndexes
       , staticProxySKHeavys

       -- Helpers
       , feedPM
       , feedPMC
       ) where

import           Universum

import           Cardano.Crypto.Wallet (xprv, xpub)
import           Crypto.Hash (Blake2b_224)
import qualified Crypto.SCRAPE as Scrape
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List (zipWith4, (!!))
import           Data.List.NonEmpty (fromList)
import qualified Data.Map as M
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Data.Time.Units (Millisecond, fromMicroseconds)
import           Data.Typeable (typeRep)
import qualified Data.Vector as V
import           Hedgehog (Gen, Property)
import qualified Hedgehog as H
import qualified Hedgehog.Gen as Gen

import           Pos.Binary.Class (Bi, Case (..), LengthOf, Raw (..),
                     SizeOverride (..), asBinary, szCases)
import           Pos.Core.Attributes (Attributes, mkAttributes)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                     AddrStakeDistribution (..), AddrType (..),
                     BlockCount (..), ChainDifficulty (..), Coeff (..),
                     Coin (..), CoinPortion (..), IsBootstrapEraAddr (..),
                     Script (..), ScriptVersion, SharedSeed (..), SlotLeaders,
                     StakeholderId, StakesList, TxFeePolicy (..),
                     TxSizeLinear (..), addressHash, makeAddress,
                     makePubKeyAddress)
import           Pos.Core.Delegation (DlgPayload (..), HeavyDlgIndex (..),
                     LightDlgIndices (..), ProxySKBlockInfo, ProxySKHeavy)
import           Pos.Core.Merkle (mkMerkleTree, mtRoot)
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..),
                     FlatSlotId, LocalSlotIndex (..), SlotCount (..),
                     SlotId (..), TimeDiff (..), Timestamp (..))
import           Pos.Core.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                     InnerSharesMap, Opening, OpeningsMap, SharesDistribution,
                     SignedCommitment, SscPayload (..), SscProof (..),
                     VssCertificate (..), VssCertificatesHash,
                     VssCertificatesMap (..), mkCommitmentsMap,
                     mkVssCertificate, mkVssCertificatesMap,
                     randCommitmentAndOpening)
import           Pos.Core.Txp (Tx (..), TxAux (..), TxId, TxIn (..),
                     TxInWitness (..), TxOut (..), TxOutAux (..),
                     TxPayload (..), TxProof (..), TxSig, TxSigData (..),
                     TxWitness, mkTxPayload)
import           Pos.Core.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                     UpAttributes, UpId, UpdateData (..), UpdatePayload (..),
                     UpdateProof, UpdateProposal, UpdateProposalToSign (..),
                     UpdateVote (..), VoteId, mkUpdateProof,
                     mkUpdateProposalWSign, mkUpdateVoteSafe)
import           Pos.Crypto (AbstractHash (..), EncShare (..),
                     HDAddressPayload (..), Hash, ProtocolMagic (..),
                     PublicKey (..), RedeemPublicKey, RedeemSignature,
                     SafeSigner (..), Secret (..), SecretKey (..),
                     SecretProof (..), SignTag (..), Signature, VssKeyPair,
                     VssPublicKey (..), abstractHash, decryptShare,
                     deterministic, deterministicVssKeyGen, hash,
                     redeemDeterministicKeyGen, redeemSign, safeCreatePsk,
                     sign, toVssPublicKey)

import           Serokell.Data.Memory.Units (Byte)

import           Test.Pos.Binary.Helpers (SizeTestConfig (..), scfg, sizeTest)
import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Crypto.Gen (genProtocolMagic)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)



--------------------------------------------------------------------------------
-- Address
--------------------------------------------------------------------------------
golden_Address :: Property
golden_Address = goldenTestBi a "test/golden/Address"
  where
    a = makeAddress exampleAddrSpendingData_PubKey attrs
    attrs = AddrAttributes hap BootstrapEraDistr
    hap = Just (HDAddressPayload (getBytes 32 32))

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
-- Helpers
--------------------------------------------------------------------------------

feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic

feedPC :: (ProtocolConstants -> H.Gen a) -> H.Gen a
feedPC genA = genA =<< genProtocolConstants

feedPMC :: (ProtocolMagic -> ProtocolConstants -> H.Gen a) -> H.Gen a
feedPMC genA = do
    pm <- genProtocolMagic
    pc <- genProtocolConstants
    genA pm pc


--------------------------------------------------------------------------------
-- Example golden datatypes
--------------------------------------------------------------------------------

exampleAttributes :: Attributes ()
exampleAttributes = mkAttributes ()

exampleBlockVersion :: BlockVersion
exampleBlockVersion = BlockVersion 1 1 1

exampleBlockVersionData :: BlockVersionData
exampleBlockVersionData = BlockVersionData
                              (999 :: ScriptVersion)
                              (999 :: Millisecond)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (999 :: Byte)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (CoinPortion 99)
                              (99 :: FlatSlotId)
                              sfrule
                              (TxFeePolicyTxSizeLinear tslin)
                              (EpochIndex 99)
    where
        tslin = TxSizeLinear c1' c2'
        c1' = Coeff (MkFixed 999)
        c2' = Coeff (MkFixed 77)
        sfrule = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleChainDifficulty :: ChainDifficulty
exampleChainDifficulty = ChainDifficulty (BlockCount 9999)

exampleCommitment :: Commitment
exampleCommitment = fst exampleCommitmentOpening

exampleCommitmentOpening :: (Commitment, Opening)
exampleCommitmentOpening =
  let numKeys   = 128 :: Int
      -- parties   = 20 :: Integer
      threshold = 15 :: Integer
      vssKeys   = replicate numKeys exampleVssPublicKey
  in  deterministic "commitmentOpening"
      $ randCommitmentAndOpening threshold (fromList vssKeys)

exampleCommitmentSignature :: CommitmentSignature
exampleCommitmentSignature =
    sign
      (ProtocolMagic 0)
      SignForTestingOnly
      exampleSecretKey
      (exampleEpochIndex, exampleCommitment)

exampleCommitmentsMap :: CommitmentsMap
exampleCommitmentsMap =
    let numCommitments = 1
        signedCommitments = replicate numCommitments exampleSignedCommitment
    in  mkCommitmentsMap signedCommitments

exampleEpochIndex :: EpochIndex
exampleEpochIndex = EpochIndex 14

exampleOpening :: Opening
exampleOpening = snd exampleCommitmentOpening

exampleOpeningsMap :: OpeningsMap
exampleOpeningsMap =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        openings = replicate mapSize exampleOpening
    in  HM.fromList $ zip stakeholderIds openings

exampleSafeSigner :: Int -> SafeSigner
exampleSafeSigner offset = staticSafeSigners!!offset

exampleSharesDistribution :: SharesDistribution
exampleSharesDistribution =
    let mapSize = 1
        stakeholderIds = replicate mapSize exampleStakeholderId
        word16s = [1337]
    in  HM.fromList $ zip stakeholderIds word16s

exampleSignedCommitment :: SignedCommitment
exampleSignedCommitment =
    (examplePublicKey, exampleCommitment, exampleCommitmentSignature)

exampleStakeholderId :: StakeholderId
exampleStakeholderId = abstractHash examplePublicKey :: StakeholderId

exampleStakeholderIds :: Int -> Int -> [StakeholderId]
exampleStakeholderIds offset l = map abstractHash $ examplePublicKeys offset l

exampleVssKeyPairs :: Int -> Int -> [VssKeyPair]
exampleVssKeyPairs offset count = map (toPair . (*offset)) [0..count]
    where
        toPair start = deterministicVssKeyGen (getBytes start 32)

exampleVssPublicKey :: VssPublicKey
exampleVssPublicKey = toVssPublicKey mkVssKeyPair
  where
    mkVssKeyPair = deterministicVssKeyGen $ (getBytes 0 32)

exampleVssPublicKeys :: Int -> Int -> [VssPublicKey]
exampleVssPublicKeys offset count = map (toKey . (*offset)) [0..count]
    where
        toKey start = toVssPublicKey . deterministicVssKeyGen $ (getBytes start 32)

exampleBlockVersionModifier :: BlockVersionModifier
exampleBlockVersionModifier = BlockVersionModifier
                              (Just (999 :: ScriptVersion))
                              (Just (999 :: Millisecond))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just (999 :: Byte))
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just $ CoinPortion 99)
                              (Just (99 :: FlatSlotId))
                              (Just sfrule')
                              (Just $ TxFeePolicyTxSizeLinear tslin')
                              (Just $ EpochIndex 99)
    where
        tslin' = TxSizeLinear co1 co2
        co1 = Coeff (MkFixed 999)
        co2 = Coeff (MkFixed 77)
        sfrule' = (SoftforkRule (CoinPortion 99) (CoinPortion 99) (CoinPortion 99))

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePublicKey

examplePublicKey :: PublicKey
examplePublicKey = pk
  where [pk] = examplePublicKeys 16 1 -- 16 could be any number, as we take the first key

examplePublicKeys :: Int -> Int -> [PublicKey]
examplePublicKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right pk = PublicKey <$> xpub (getBytes start 64)
                   in pk

exampleRedeemPublicKey :: RedeemPublicKey
exampleRedeemPublicKey = fromJust (fst <$> redeemDeterministicKeyGen (getBytes 0 32))

exampleRedeemSignature :: RedeemSignature TxSigData
exampleRedeemSignature = redeemSign (ProtocolMagic 0) SignForTestingOnly rsk exampleTxSigData
    where
        rsk = fromJust (snd <$> redeemDeterministicKeyGen (getBytes 0 32))

-- In order to get the key starting at byte 10, we generate two with offsets of 10
-- between them and take the second.
exampleSecretKey :: SecretKey
exampleSecretKey = (exampleSecretKeys 10 2) !! 1

exampleSecretKeys :: Int -> Int -> [SecretKey]
exampleSecretKeys offset count = map (toKey . (*offset)) [0..count-1]
  where
    toKey start = let Right sk = SecretKey <$> xprv (getBytes start 128)
                   in sk

-- Lifted from genSharedSecret in `Pos.Crypto.SecretSharing`.
-- added `deterministic` in third guard.

exampleSharedSecret
    :: Scrape.Threshold -> NonEmpty VssPublicKey -> (Secret, SecretProof, [(VssPublicKey, EncShare)])
exampleSharedSecret t ps
    | t <= 1     = error "genSharedSecret: threshold must be > 1"
    | t >= n - 1 = error "genSharedSecret: threshold must be > n-1"
    | otherwise  = convertRes . deterministic "ss" $ Scrape.escrow t (coerce sorted)
  where
    n = fromIntegral (length ps)
    sorted = sort (toList ps)
    convertRes (gen, secret, shares, comms, proof, pproofs) =
        (coerce secret,
         SecretProof gen proof pproofs comms,
         zip sorted (coerce shares))

-- Not sure why you don't use `VssPublicKey` for the `InnerSharesMap`
-- as you use `VssPublicKey`s to generate `DecShare`s.

exampleInnerSharesMap :: Scrape.Threshold -> Int -> InnerSharesMap
exampleInnerSharesMap count offset =
    HM.fromList $ zipWith
                      (\x y -> ((addressHash x), fromList [asBinary y]))
                          (pubKeys)
                          (decShares)
        where
          -- generate VssPublicKey and VssSecretKey pairs.
          vssKeyPairs = exampleVssKeyPairs offset $ fromIntegral (count+1)
          -- generate SharedSecrets from the VssPublicKeys generated above.
          ss = exampleSharedSecret (count) (fromList $ map toVssPublicKey vssKeyPairs)
          -- filter `VssPublicKeys`s and their corresponding `EncShare`s.
          encShares (_, _, pKeSlist) = map snd pKeSlist
          -- generate `PublicKey`s
          pubKeys = examplePublicKeys 1 $ fromIntegral (count+1)
          -- generate `DecShares`
          decShares =
            [deterministic "ss" $ decryptShare pr es | pr <- vssKeyPairs, es <- encShares ss]

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakesList :: StakesList
exampleStakesList = zip sis coins
  where
    sis   = map abstractHash (examplePublicKeys 15 3)
    coins = map Coin [79, 44, 9999999]

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = map abstractHash (fromList (examplePublicKeys 16 3))

exampleSystemTag :: SystemTag
exampleSystemTag = (exampleSystemTags 0 1) !! 0

exampleSystemTags :: Int -> Int -> [SystemTag]
exampleSystemTags offset count = map (toSystemTag . (*offset)) [0..count-1]
  where
    toSystemTag start = SystemTag (getText start 16)

exampleTxAux :: TxAux
exampleTxAux = TxAux tx exampleTxWitness
  where
    tx = UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ())

exampleTxId :: TxId
exampleTxId = exampleHashTx

exampleTxInList :: (NonEmpty TxIn)
exampleTxInList = fromList [exampleTxInUtxo]

exampleTxInUnknown :: TxIn
exampleTxInUnknown = TxInUnknown 47 ("forty seven" :: ByteString)

exampleTxInUtxo :: TxIn
exampleTxInUtxo = TxInUtxo exampleHashTx 47 -- TODO: loop here

exampleTxOut :: TxOut
exampleTxOut = TxOut (makePubKeyAddress (IsBootstrapEraAddr True) pkey) (Coin 47)
    where
        Right pkey = PublicKey <$> xpub (getBytes 0 64)

exampleTxOutList :: (NonEmpty TxOut)
exampleTxOutList = fromList [exampleTxOut]

exampleTxProof :: TxProof
exampleTxProof = TxProof 32 mroot hashWit
  where
    mroot = mtRoot $ mkMerkleTree [(UnsafeTx exampleTxInList exampleTxOutList (mkAttributes ()))]
    hashWit = hash $ [(V.fromList [(PkWitness examplePublicKey exampleTxSig)])]

exampleTxSig :: TxSig
exampleTxSig = sign (ProtocolMagic 0) SignForTestingOnly exampleSecretKey exampleTxSigData

exampleTxSigData :: TxSigData
exampleTxSigData = TxSigData exampleHashTx

exampleTxWitness :: TxWitness
exampleTxWitness = V.fromList [(PkWitness examplePublicKey exampleTxSig)]

exampleUpAttributes :: UpAttributes
exampleUpAttributes = exampleAttributes

exampleUpdateData :: UpdateData
exampleUpdateData = (exampleUpdateDatas 10 2) !! 1

exampleUpdateDatas :: Int -> Int -> [UpdateData]
exampleUpdateDatas offset count = map (toUpdateData . (*offset)) [0..count-1]
  where
    toUpdateData start =
      let h = hash $ Raw (getBytes start 128)
      in  UpdateData h h h h

exampleUpId :: UpId
exampleUpId = hash exampleUpdateProposal

exampleUpdatePayload :: UpdatePayload
exampleUpdatePayload = UpdatePayload up uv
  where
    up = Just exampleUpdateProposal
    uv = [exampleUpdateVote]

exampleUpdateProof :: UpdateProof
exampleUpdateProof = mkUpdateProof exampleUpdatePayload

exampleUpdateProposal :: UpdateProposal
exampleUpdateProposalToSign :: UpdateProposalToSign
(exampleUpdateProposal, exampleUpdateProposalToSign) =
    ( mkUpdateProposalWSign pm bv bvm sv hm ua ss
    , UpdateProposalToSign bv bvm sv hm ua )
  where
    pm  = ProtocolMagic 0
    bv  = exampleBlockVersion
    bvm = exampleBlockVersionModifier
    sv  = exampleSoftwareVersion
    hm  = HM.fromList $ zip (exampleSystemTags 10 5) (exampleUpdateDatas 10 5)
    ua  = exampleUpAttributes
    ss  = exampleSafeSigner 0

exampleUpdateVote :: UpdateVote
exampleUpdateVote = mkUpdateVoteSafe pm ss ui ar
  where
    pm = ProtocolMagic 0
    ss = exampleSafeSigner 0
    ui = exampleUpId
    ar = True

-- | ```type VoteId = (UpId, PublicKey, Bool)```
exampleVoteId :: VoteId
exampleVoteId = (exampleUpId, examplePublicKey, False)

exampleVssCertificate :: VssCertificate
exampleVssCertificate =
    mkVssCertificate
        (ProtocolMagic 0)
        exampleSecretKey
        (asBinary (toVssPublicKey $ deterministicVssKeyGen ("golden" :: ByteString)))
        (EpochIndex 11)

exampleVssCertificates :: Int -> Int -> [VssCertificate]
exampleVssCertificates offset num =  map vssCert [0..num-1]
    where
        secretKeyList = (exampleSecretKeys offset num)
        vssCert index = mkVssCertificate
                           (ProtocolMagic 0)
                           (secretKeyList !! index)
                           (asBinary (toVssPublicKey $ deterministicVssKeyGen (getBytes index 128)))
                           (EpochIndex 122)

exampleVssCertificatesMap :: Int -> Int -> VssCertificatesMap
exampleVssCertificatesMap offset num = mkVssCertificatesMap $ exampleVssCertificates offset num


exampleVssCertificatesHash :: Int -> Int -> VssCertificatesHash
exampleVssCertificatesHash offset len =
    hash . getVssCertificatesMap $ exampleVssCertificatesMap offset len


staticHeavyDlgIndexes :: [HeavyDlgIndex]
staticHeavyDlgIndexes = map (HeavyDlgIndex . EpochIndex) [5,1,3,27,99,247]

staticProtocolMagics :: [ProtocolMagic]
staticProtocolMagics = map ProtocolMagic [0..5]

staticProxySKHeavys :: [ProxySKHeavy]
staticProxySKHeavys = zipWith4 safeCreatePsk
                               staticProtocolMagics staticSafeSigners
                               (examplePublicKeys 1 6) staticHeavyDlgIndexes

staticSafeSigners :: [SafeSigner]
staticSafeSigners = map FakeSigner (exampleSecretKeys 1 6)

-- | Changing existing values in this string will break existing golden
-- tests, but it us OK to append more data to the end.
staticText :: Text
staticText
    = "Kmyw4lDSE5S4fSH6etNouiXezCyEjKc3tG4ja0kFjO8qzai26ZMPUEJfEy15ox5kJ0uKD\
    \bi7i6dLXkuesVZ9JfHgjrctsLFt2NvovXnchsOvX05Y6LohlTNt5mkPFhUoXu1EZSJTIy\
    \3fTU53b412r4AEusD7tcdRgH47yTr5hMO63bJnYBbmNperLHfiT1lP0MLQLh1J1DfoYBs\
    \auoJOzvtAgvjHo6UFttnK6vZ3Cknpuob6uMS2MkJKmuoQsqsAYcRDWbJ2Rgw4bm2ndTM4\
    \zFfuRDKvdrL6sDkuPNPYqxMWlqnXjSbU0eLtceZuKgXLHR8cdvsEvywt4JaZUQhnbq3Vl\
    \7nZqcXdoi4XGTCgSGcGp8N0SDVhvkVh0QF1RVpWPnOMyYISJvuaHfo1zXMdq9tEdtJfID"

getText :: Int -> Int -> Text
getText offset len = T.take len $ T.drop offset staticText

exampleTxPayload :: TxPayload
exampleTxPayload = mkTxPayload [exampleTxAux]

exampleSoftwareVersion :: SoftwareVersion
exampleSoftwareVersion = SoftwareVersion (ApplicationName "Golden") 99

exampleHashTx :: Hash Tx
exampleHashTx = coerce (hash "golden" :: Hash Text)

exampleSscProof :: SscProof
exampleSscProof = CommitmentsProof (hash exampleCommitmentsMap)
                                   (exampleVssCertificatesHash 10 4)

exampleSscPayload :: SscPayload
exampleSscPayload = SharesPayload exampleSharesMap (exampleVssCertificatesMap 10 4)
  where
    exampleSharesMap = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

exampleProxySKBlockInfo :: ProxySKBlockInfo
exampleProxySKBlockInfo = Just (staticProxySKHeavys !! 0, examplePublicKey)

exampleLightDlgIndices :: LightDlgIndices
exampleLightDlgIndices = LightDlgIndices (EpochIndex 7, EpochIndex 88)

sizeEstimates :: H.Group
sizeEstimates =
  let check :: forall a. (Show a, Bi a) => Gen a -> Property
      check g = sizeTest $ scfg { gen = g }
      pm = ProtocolMagic 0
      knownTxIn (TxInUnknown _ _) = False
      knownTxIn _                 = True
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
      txSigSize    = (typeRep (Proxy @(Signature TxSigData))
                     , SizeConstant 66)
      scriptSize   = (typeRep (Proxy @Script),
                      SizeConstant $ szCases [ Case "loScript" 1
                                             , Case "hiScript" 255 ])

  in H.Group "Encoded size bounds for core types."
        [ ("Coin"                 , check genCoin)
        , ("BlockCount"           , check genBlockCount)
        , ("TxId"                 , check genTxId)
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
        , ("Tx"                   , sizeTest $ scfg
              { gen = genTx
              , addlCtx = M.fromList [ attrUnitSize, attrAddrSize ]
              , computedCtx = \tx -> M.fromList
                  [ (typeRep (Proxy @(LengthOf [TxIn])),
                     SizeConstant (fromIntegral $ length $ _txInputs tx))
                  , (typeRep (Proxy @(LengthOf [TxOut])),
                     SizeConstant (fromIntegral $ length $ _txOutputs tx))
                  ]
              })
        , ("TxIn"                 , check (Gen.filter knownTxIn genTxIn))
        , ("TxOut"                , sizeTest $ scfg
              { gen = genTxOut
              , addlCtx = M.fromList [ attrAddrSize ]
              })
        , ("TxAux"                , sizeTest $ scfg
              { gen = genTxAux pm
              , addlCtx = M.fromList [ attrUnitSize
                                     , attrAddrSize
                                     , scriptSize
                                     , txSigSize ]
              , computedCtx = \(TxAux tx witness) -> M.fromList
                  [ (typeRep (Proxy @(LengthOf [TxIn])),
                     SizeConstant (fromIntegral $ length $ _txInputs tx))
                  , (typeRep (Proxy @(LengthOf (Vector TxInWitness))),
                     SizeConstant (fromIntegral $ length witness))
                  , (typeRep (Proxy @(LengthOf [TxOut])),
                     SizeConstant (fromIntegral $ length $ _txOutputs tx))
                  ]
              })
        , ("TxInWitness"          , sizeTest $ scfg
              { gen = genTxInWitness pm
              , addlCtx = M.fromList [ txSigSize, scriptSize ]
              })
        , ("TxSigData"            , check genTxSigData)
        , ("Signature TxSigData"  , sizeTest $ scfg
              { gen = genTxSig pm
              , addlCtx = M.fromList [ txSigSize ]
              })
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
