module Test.Pos.Core.Bi
    -- ( tests
    -- , roundTripAddressBi
    -- ) where
    where

import           Universum

import           Cardano.Crypto.Wallet (xprv, xpub)
import           Data.Coerce (coerce)
import           Data.Fixed (Fixed (..))
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (fromList)
import qualified Data.Map as M
import           Data.Time.Units (fromMicroseconds)
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Core.Block (GenesisBody (..), GenesisProof (..), HeaderHash, mkGenesisHeader)
import           Pos.Core.Common (AddrAttributes (..), AddrSpendingData (..),
                                  AddrStakeDistribution (..), AddrType (..), BlockCount (..),
                                  ChainDifficulty (..), Coeff (..), Coin (..), CoinPortion (..),
                                  IsBootstrapEraAddr (..), Script (..), ScriptVersion,
                                  SharedSeed (..), SlotLeaders, StakeholderId, StakesList,
                                  TxFeePolicy (..), TxSizeLinear (..), makeAddress,
                                  makePubKeyAddress)
import           Pos.Core.Configuration (GenesisHash (..))
import           Pos.Core.ProtocolConstants (ProtocolConstants (..))
import           Pos.Core.Slotting (EpochIndex (..), EpochOrSlot (..), FlatSlotId,
                                    LocalSlotIndex (..), SlotCount (..), SlotId (..), TimeDiff (..),
                                    Timestamp (..))
import           Pos.Core.Txp (Tx (..), TxId, TxIn (..), TxInWitness (..), TxOut (..),
                               TxOutAux (..), TxSig, TxSigData (..))
import           Pos.Crypto (HDAddressPayload (..), Hash, ProtocolMagic (..), PublicKey (..),
                             SecretKey (..), SignTag (..), hash, redeemDeterministicKeyGen,
                             redeemSign, sign)
import           Pos.Crypto.Hashing (AbstractHash (..), abstractHash)
import           Pos.Data.Attributes (mkAttributes)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (discoverGolden, discoverRoundTrip, eachOf,
                                                          goldenTestBi, roundTripsBiBuildable,
                                                          roundTripsBiShow)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)
import           Test.Pos.Crypto.Gen (genProtocolMagic)

--------------------------------------------------------------------------------
-- Pos.Core.Block
--------------------------------------------------------------------------------

-- BlockBodyAttributes
golden_BlockBodyAttributes :: Property
golden_BlockBodyAttributes = goldenTestBi bba "test/golden/BlockBodyAttributes"
  where
    bba = mkAttributes ()

roundTripBlockBodyAttributesBi :: Property
roundTripBlockBodyAttributesBi = eachOf 1000 genBlockBodyAttributes roundTripsBiBuildable

-- BlockHeader
golden_BlockHeaderGenesis :: Property
golden_BlockHeaderGenesis = goldenTestBi bhg "test/golden/BlockHeaderGenesis"
  where
    bhg = mkGenesisHeader (ProtocolMagic 0)
                          (Left (GenesisHash prevHash))
                          (EpochIndex 11)
                          genesisBody
    genesisBody = GenesisBody (sId1 :| [sId2])
    prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a
    sId1 = coerce (hash ("stakeholder 1" :: Text))
    sId2 = coerce (hash ("stakeholder 2" :: Text))

-- golden_BlockHeaderMain :: Property
-- golden_BlockHeaderMain = goldenTestBi bhm "test/golden/BlockHeaderMain"
--   where
--     bhm = mkMainHeaderExplicit (ProtocolMagic 0) prevHash
--                                (const 5) exampleSlotId
--                                sk pske
--                                body extra
--     prevHash = coerce (hash ("genesisHash" :: Text)) :: Hash a
--     sk = undefined
--     pske = undefined
--     body = undefined
--     extra = undefined

roundTripBlockHeaderBi :: Property
roundTripBlockHeaderBi = eachOf 10 (feedPMC genBlockHeader) roundTripsBiBuildable

roundTripBlockHeaderAttributesBi :: Property
roundTripBlockHeaderAttributesBi = eachOf 1000 genBlockHeaderAttributes roundTripsBiBuildable

roundTripBlockSignatureBi :: Property
roundTripBlockSignatureBi = eachOf 10 (feedPMC genBlockSignature) roundTripsBiBuildable

roundTripGenesisBlockHeaderBi :: Property
roundTripGenesisBlockHeaderBi = eachOf 1000 (feedPM genGenesisBlockHeader) roundTripsBiBuildable

roundTripGenesisBodyBi :: Property
roundTripGenesisBodyBi = eachOf 1000 genGenesisBody roundTripsBiShow

roundTripGenesisConsensusDataBi :: Property
roundTripGenesisConsensusDataBi = eachOf 1000 genGenesisConsensusData roundTripsBiShow

-- GenesisHash is just a newtype around a Hash, and lacks Bi instances. The newtype is
-- unwrapped when constructing a block, so it doesn't appear anywhere and we don't need
-- to test it.

-- HeaderHash
golden_HeaderHash :: Property
golden_HeaderHash = goldenTestBi hh "test/golden/HeaderHash"
  where hh = coerce (hash ("HeaderHash" :: Text)) :: HeaderHash

roundTripHeaderHashBi :: Property
roundTripHeaderHashBi = eachOf 1000 genHeaderHash roundTripsBiBuildable

golden_GenesisProof :: Property
golden_GenesisProof = goldenTestBi gp "test/golden/GenesisProof"
  where gp = GenesisProof (abstractHash exampleSlotLeaders)

roundTripGenesisProofBi :: Property
roundTripGenesisProofBi = eachOf 1000 genGenesisProof roundTripsBiBuildable

roundTripMainBlockHeaderBi :: Property
roundTripMainBlockHeaderBi = eachOf 20 (feedPMC genMainBlockHeader) roundTripsBiBuildable

roundTripMainBodyBi :: Property
roundTripMainBodyBi = eachOf 20 (feedPM genMainBody) roundTripsBiShow

roundTripMainConsensusData :: Property
roundTripMainConsensusData = eachOf 20 (feedPMC genMainConsensusData) roundTripsBiShow

roundTripMainExtraBodyDataBi :: Property
roundTripMainExtraBodyDataBi = eachOf 1000 genMainExtraBodyData roundTripsBiBuildable

roundTripMainExtraHeaderDataBi :: Property
roundTripMainExtraHeaderDataBi = eachOf 1000 genMainExtraHeaderData roundTripsBiBuildable

roundTripMainProofBi :: Property
roundTripMainProofBi = eachOf 20 (feedPM genMainProof) roundTripsBiBuildable

roundTripMainToSignBi :: Property
roundTripMainToSignBi = eachOf 20 (feedPMC genMainToSign) roundTripsBiShow


-- group 1

-- TODO mhueschen grok why this doesn't have a Bi instance, but (Attributes AddrAttributes) does
-- ^ see module Pos.Core.Common.AddrAttributes
-- roundTripAddrAttributesBi :: Property
-- roundTripAddrAttributesBi = eachOf 1000 genAddrAttributes roundTripsBiBuildable

golden_Address :: Property
golden_Address = goldenTestBi a "test/golden/Address"
  where
    a = makeAddress exampleAddrSpendingData_PubKey attrs
    attrs = AddrAttributes hap BootstrapEraDistr
    hap = Just (HDAddressPayload (getBytes 32 32))

roundTripAddressBi :: Property
roundTripAddressBi = eachOf 1000 genAddress roundTripsBiBuildable

-- AddrSpendingData
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

-- AddrStakeDistribution
golden_AddrStakeDistribution_Bootstrap :: Property
golden_AddrStakeDistribution_Bootstrap =
    goldenTestBi BootstrapEraDistr "test/golden/AddrStakeDistribution_Bootstrap"

golden_AddrStakeDistribution_SingleKey :: Property
golden_AddrStakeDistribution_SingleKey =
    goldenTestBi asd "test/golden/AddrStakeDistribution_SingleKey"
  where
    asd = SingleKeyDistr (abstractHash examplePubicKey)

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

-- AddrType
golden_AddrType_PK :: Property
golden_AddrType_PK = goldenTestBi at "test/golden/AddrType_PK"
  where at = ATPubKey

golden_AddrType_S :: Property
golden_AddrType_S = goldenTestBi at "test/golden/AddrType_S"
  where at = ATScript

golden_AddrType_R :: Property
golden_AddrType_R = goldenTestBi at "test/golden/AddrType_R"
  where at = ATRedeem

golden_AddrType_U :: Property
golden_AddrType_U = goldenTestBi at "test/golden/AddrType_U"
  where at = ATUnknown 57

roundTripAddrTypeBi :: Property
roundTripAddrTypeBi = eachOf 1000 genAddrType roundTripsBiShow

-- BlockCount
golden_BlockCount :: Property
golden_BlockCount = goldenTestBi bc "test/golden/BlockCount"
  where bc = BlockCount 999

roundTripBlockCountBi :: Property
roundTripBlockCountBi = eachOf 1000 genBlockCount roundTripsBiBuildable

-- ChainDifficulty
golden_ChainDifficulty :: Property
golden_ChainDifficulty = goldenTestBi cd "test/golden/ChainDifficulty"
  where cd = ChainDifficulty (BlockCount 9999)

roundTripChainDifficultyBi :: Property
roundTripChainDifficultyBi = eachOf 1000 genChainDifficulty roundTripsBiBuildable

-- Coeff
golden_Coeff :: Property
golden_Coeff = goldenTestBi c "test/golden/Coeff"
  where c = Coeff (MkFixed 101)

roundTripCoeffBi :: Property
roundTripCoeffBi = eachOf 1000 genCoeff roundTripsBiBuildable

-- Coin
golden_Coin :: Property
golden_Coin = goldenTestBi c "test/golden/Coin"
  where c = Coin 9732

roundTripCoinBi :: Property
roundTripCoinBi = eachOf 1000 genCoin roundTripsBiBuildable

-- CoinPortion
golden_CoinPortion :: Property
golden_CoinPortion = goldenTestBi c "test/golden/CoinPortion"
  where c = CoinPortion 9702

roundTripCoinPortionBi :: Property
roundTripCoinPortionBi = eachOf 1000 genCoinPortion roundTripsBiBuildable

-- Script
golden_Script :: Property
golden_Script = goldenTestBi exampleScript "test/golden/Script"

roundTripScriptBi :: Property
roundTripScriptBi = eachOf 1000 genScript roundTripsBiBuildable

-- ScriptVersion
golden_ScriptVersion :: Property
golden_ScriptVersion = goldenTestBi sv "test/golden/ScriptVersion"
  where sv = 6001 :: ScriptVersion

roundTripScriptVersionBi :: Property
roundTripScriptVersionBi = eachOf 1000 genScriptVersion roundTripsBiBuildable

-- SharedSeed
golden_SharedSeed :: Property
golden_SharedSeed = goldenTestBi s "test/golden/SharedSeed"
  where s = SharedSeed (getBytes 8 32)

roundTripSharedSeedBi :: Property
roundTripSharedSeedBi = eachOf 1000 genSharedSeed roundTripsBiBuildable

-- SlotLeaders
golden_SlotLeaders :: Property
golden_SlotLeaders = goldenTestBi exampleSlotLeaders "test/golden/SlotLeaders"

roundTripSlotLeadersBi :: Property
roundTripSlotLeadersBi = eachOf 1000 genSlotLeaders roundTripsBiShow

-- StakeholderId
golden_StakeholderId :: Property
golden_StakeholderId = goldenTestBi si "test/golden/StakeholderId"
  where
    si = abstractHash pk :: StakeholderId
    Right pk = PublicKey <$> xpub (getBytes  0 64)

roundTripStakeholderIdBi :: Property
roundTripStakeholderIdBi = eachOf 1000 genStakeholderId roundTripsBiBuildable

-- StakesList
golden_StakesList :: Property
golden_StakesList = goldenTestBi exampleStakesList "test/golden/StakesList"

roundTripStakesListBi :: Property
roundTripStakesListBi = eachOf 1000 genStakesList roundTripsBiShow

-- StakesMap
golden_StakesMap :: Property
golden_StakesMap = goldenTestBi sm "test/golden/StakesMap"
  where sm = HM.fromList exampleStakesList

roundTripStakesMapBi :: Property
roundTripStakesMapBi = eachOf 1000 genStakesMap roundTripsBiShow

-- TxFeePolicy
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

-- TxSizeLinear
golden_TxSizeLinear :: Property
golden_TxSizeLinear = goldenTestBi tsl "test/golden/TxSizeLinear"
  where
    tsl = TxSizeLinear c1 c2
    c1 = Coeff (MkFixed 999)
    c2 = Coeff (MkFixed 77)

roundTripTxSizeLinearBi :: Property
roundTripTxSizeLinearBi = eachOf 1000 genTxSizeLinear roundTripsBiBuildable

-- group 2

-- no Bi instance
-- roundTripGenesisConfigurationBi :: Property
-- roundTripGenesisConfigurationBi = eachOf 1000 genGenesisConfiguration roundTripsBiBuildable

-- no Bi instance
-- roundTripCoreConfigurationBi :: Property
-- roundTripCoreConfigurationBi = eachOf 1000 genCoreConfiguration roundTripsBiBuildable

roundTripDlgPayloadBi :: Property
roundTripDlgPayloadBi = eachOf 100 (feedPM genDlgPayload) roundTripsBiBuildable

roundTripHeavyDlgIndexBi :: Property
roundTripHeavyDlgIndexBi = eachOf 1000 genHeavyDlgIndex roundTripsBiBuildable

roundTripLightDlgIndicesBi :: Property
roundTripLightDlgIndicesBi = eachOf 1000 genLightDlgIndices roundTripsBiBuildable

roundTripProxySKBlockInfoBi :: Property
roundTripProxySKBlockInfoBi = eachOf 200 (feedPM genProxySKBlockInfo) roundTripsBiShow

roundTripProxySKHeavyBi :: Property
roundTripProxySKHeavyBi = eachOf 200 (feedPM genProxySKHeavy) roundTripsBiBuildable

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

-- EpochIndex
golden_EpochIndex :: Property
golden_EpochIndex = goldenTestBi ei "test/golden/EpochIndex"
  where ei = EpochIndex 14

roundTripEpochIndexBi :: Property
roundTripEpochIndexBi = eachOf 1000 genEpochIndex roundTripsBiBuildable

-- EpochOrSlot
golden_EpochOrSlotEI :: Property
golden_EpochOrSlotEI = goldenTestBi eos "test/golden/EpochOrSlotEI"
  where eos = EpochOrSlot (Left (EpochIndex 14))

golden_EpochOrSlotSI :: Property
golden_EpochOrSlotSI = goldenTestBi eos "test/golden/EpochOrSlotSI"
  where eos = EpochOrSlot (Right exampleSlotId)

roundTripEpochOrSlotBi :: Property
roundTripEpochOrSlotBi = eachOf 1000 (feedPC genEpochOrSlot) roundTripsBiBuildable

-- FlatSlotId
golden_FlatSlotId :: Property
golden_FlatSlotId = goldenTestBi fsi "test/golden/FlatSlotId"
  where fsi = 5001 :: FlatSlotId

roundTripFlatSlotIdBi :: Property
roundTripFlatSlotIdBi = eachOf 1000 genFlatSlotId roundTripsBiBuildable

-- LocalSlotIndex
golden_LocalSlotIndex :: Property
golden_LocalSlotIndex = goldenTestBi lsi "test/golden/LocalSlotIndex"
  where lsi = UnsafeLocalSlotIndex 52

roundTripLocalSlotIndexBi :: Property
roundTripLocalSlotIndexBi = eachOf 1000 (feedPC genLocalSlotIndex) roundTripsBiBuildable

-- SlotCount
golden_SlotCount :: Property
golden_SlotCount = goldenTestBi sc "test/golden/SlotCount"
  where sc = SlotCount 474747

roundTripSlotCountBi :: Property
roundTripSlotCountBi = eachOf 1000 genSlotCount roundTripsBiBuildable

-- SlotId
golden_SlotId :: Property
golden_SlotId = goldenTestBi exampleSlotId "test/golden/SlotId"

roundTripSlotIdBi :: Property
roundTripSlotIdBi = eachOf 1000 (feedPC genSlotId) roundTripsBiBuildable

-- TimeDiff
golden_TimeDiff :: Property
golden_TimeDiff = goldenTestBi td "test/golden/TimeDiff"
  where td = TimeDiff 4747

roundTripTimeDiffBi :: Property
roundTripTimeDiffBi = eachOf 1000 genTimeDiff roundTripsBiBuildable


-- JORDAN's TYPES

--------------------------------------------------------------------------------
-- ApplicationName
--------------------------------------------------------------------------------

roundTripApplicationName :: Property
roundTripApplicationName = eachOf 10 genApplicationName roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Attributes
--------------------------------------------------------------------------------

roundTripAttributes :: Property
roundTripAttributes = eachOf 10 (genAttributes (pure ())) roundTripsBiShow

--------------------------------------------------------------------------------
-- BlockVersion
--------------------------------------------------------------------------------

roundTripBlockVersion :: Property
roundTripBlockVersion = eachOf 10 genBlockVersion roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionData :: Property
roundTripBlockVersionData = eachOf 10 genBlockVersionData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- BlockVersionData
--------------------------------------------------------------------------------

roundTripBlockVersionModifier :: Property
roundTripBlockVersionModifier = eachOf 10 genBlockVersionModifier roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

-- Come back to this golden test after bitripping tests are finished.
--golden_Commitment :: Property
--golden_Commitment = goldenTestBi commitment "test/golden/Commitment"
--    where
--        commitment =

roundTripCommitment :: Property
roundTripCommitment = eachOf 10 genCommitment roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

roundTripCommitmentsMap :: Property
roundTripCommitmentsMap = eachOf 10 (genCommitmentsMap $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsSignature
--------------------------------------------------------------------------------

roundTripCommitmentSignature :: Property
roundTripCommitmentSignature = eachOf 10 (genCommitmentSignature $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- HashRaw
--------------------------------------------------------------------------------

roundTripHashRaw :: Property
roundTripHashRaw = eachOf 10 genHashRaw roundTripsBiBuildable

--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

roundTripInnerSharesMap :: Property
roundTripInnerSharesMap = eachOf 10 genInnerSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleTree
--------------------------------------------------------------------------------

roundTripMerkleTree :: Property
roundTripMerkleTree = eachOf 10 (genMerkleTree genHashRaw) roundTripsBiShow

--------------------------------------------------------------------------------
-- MerkleRoot
--------------------------------------------------------------------------------

roundTripMerkleRoot :: Property
roundTripMerkleRoot = eachOf 10 (genMerkleRoot genHashRaw) roundTripsBiBuildable


--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

roundTripOpening :: Property
roundTripOpening = eachOf 10 genOpening roundTripsBiBuildable

--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

roundTripOpeningsMap :: Property
roundTripOpeningsMap = eachOf 10 genOpeningsMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

roundTripSignedCommitment :: Property
roundTripSignedCommitment = eachOf 10 (genSignedCommitment $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesDistribution
--------------------------------------------------------------------------------

roundTripSharesDistribution :: Property
roundTripSharesDistribution = eachOf 10 genSharesDistribution roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

roundTripSharesMap :: Property
roundTripSharesMap = eachOf 10 genSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SoftforkRule
--------------------------------------------------------------------------------

roundTripSoftforkRule :: Property
roundTripSoftforkRule = eachOf 10 genSoftforkRule roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SoftwareVersion
--------------------------------------------------------------------------------

roundTripSoftwareVersion :: Property
roundTripSoftwareVersion = eachOf 10 genSoftwareVersion roundTripsBiBuildable


--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

roundTripSscPayload :: Property
roundTripSscPayload = eachOf 10 (feedPM genSscPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

roundTripSscProof :: Property
roundTripSscProof = eachOf 10 (genSscProof $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SystemTag
--------------------------------------------------------------------------------

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
roundTripTimestamp = eachOf 10 genTimestamp roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx
--------------------------------------------------------------------------------

roundTripTx :: Property
roundTripTx = eachOf 10 genTx roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAttributes
--------------------------------------------------------------------------------

roundTripTxAttributes :: Property
roundTripTxAttributes = eachOf 10 genTxAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxAux
--------------------------------------------------------------------------------

-- fails
roundTripTxAux :: Property
roundTripTxAux = eachOf 1000 (feedPM genTxAux) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- Tx Hash
--------------------------------------------------------------------------------

golden_HashTx :: Property
golden_HashTx = goldenTestBi hashTx "test/golden/PkWitness"

roundTripHashTx :: Property
roundTripHashTx = eachOf 10 genTxHash roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxIn
--------------------------------------------------------------------------------


golden_TxInUtxo :: Property
golden_TxInUtxo = goldenTestBi txInUtxo "test/golden/TxInUtxo"

golden_TxInUnknown :: Property
golden_TxInUnknown = goldenTestBi txInUnknown "test/golden/TxInUnknown"

roundTripTxIn :: Property
roundTripTxIn = eachOf 10 genTxIn roundTripsBiBuildable


--------------------------------------------------------------------------------
-- TxId
--------------------------------------------------------------------------------

golden_TxId :: Property
golden_TxId = goldenTestBi txId "test/golden/TxId"

roundTripTxId :: Property
roundTripTxId = eachOf 10 genTxId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxInList
--------------------------------------------------------------------------------

golden_TxInUtxoList :: Property
golden_TxInUtxoList = goldenTestBi txInUtxoList "test/golden/TxInUtxoList"

golden_TxInUnkownList :: Property
golden_TxInUnkownList = goldenTestBi txInUnknownList "test/golden/TxInUnknownList"


roundTripTxInList :: Property
roundTripTxInList = eachOf 10 genTxInList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxInWitness
--------------------------------------------------------------------------------

golden_PkWitness :: Property
golden_PkWitness = goldenTestBi pkWitness "test/golden/PkWitness"
     where
        pkWitness = PkWitness pubKey txSig
        Right pubKey = PublicKey <$> xpub (getBytes 0 64)

golden_ScriptWitness :: Property
golden_ScriptWitness = goldenTestBi scriptWitness "text/golden/ScriptWitness"
    where
        scriptWitness = ScriptWitness validatorScript redeemerScript
        validatorScript = Script 47 "serialized script"
        redeemerScript = Script 47 "serialized script"


golden_RedeemWitness :: Property
golden_RedeemWitness = goldenTestBi redeemWitness "test/golden/RedeemWitness"
    where
        redeemWitness = RedeemWitness redeemPublicKey redeemSig
        Just redeemPublicKey = fst <$> redeemDeterministicKeyGen (getBytes 0 32)
        redeemSig = redeemSign (ProtocolMagic 0) SignForTestingOnly rsk txSigData
        Just rsk = snd <$> redeemDeterministicKeyGen (getBytes 0 32)

golden_UnknownWitnessType :: Property
golden_UnknownWitnessType = goldenTestBi unkWitType "test/golden/UnknownWitnessType"
    where
        unkWitType = UnknownWitnessType 47 "forty seven"

-- 4000 because this should generate 1000 for each constructor

roundTripTxInWitness :: Property
roundTripTxInWitness = eachOf 10 (genTxInWitness $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutList
--------------------------------------------------------------------------------

golden_TxOutList :: Property
golden_TxOutList = goldenTestBi txOutList "test/golden/TxOutList"

roundTripTxOutList :: Property
roundTripTxOutList = eachOf 10 genTxOutList roundTripsBiShow

--------------------------------------------------------------------------------
-- TxOut
--------------------------------------------------------------------------------

golden_TxOut :: Property
golden_TxOut = goldenTestBi txOut "test/golden/TxOut"

roundTripTxOut :: Property
roundTripTxOut = eachOf 10 genTxOut roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxOutAux
--------------------------------------------------------------------------------

golden_TxOutAux :: Property
golden_TxOutAux =  goldenTestBi txOutAux "test/golden/TxOutAux"
    where
        txOutAux = TxOutAux txOut

roundTripTxOutAux :: Property
roundTripTxOutAux = eachOf 10 genTxOutAux roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxPayload
--------------------------------------------------------------------------------

-- fails
roundTripTxPayload :: Property
roundTripTxPayload = eachOf 1000 (feedPM genTxPayload) roundTripsBiShow

--------------------------------------------------------------------------------
-- TxProof
--------------------------------------------------------------------------------

roundTripTxProof :: Property
roundTripTxProof = eachOf 10 (genTxProof $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSig
--------------------------------------------------------------------------------

golden_TxSig :: Property
golden_TxSig = goldenTestBi txSigGold "test/golden/TxSig"
    where
        txSigGold = sign (ProtocolMagic 0) SignForTestingOnly skey txSigData
        Right skey = SecretKey <$> xprv (getBytes 10 128)

roundTripTxSig :: Property
roundTripTxSig = eachOf 10 (genTxSig $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- TxSigData
--------------------------------------------------------------------------------

golden_TxSigData :: Property
golden_TxSigData = goldenTestBi txSigData "test/golden/TxSigData"

roundTripTxSigData :: Property
roundTripTxSigData = eachOf 10 genTxSigData roundTripsBiShow

--------------------------------------------------------------------------------
-- TxWitness
--------------------------------------------------------------------------------

roundTripTxWitness :: Property
roundTripTxWitness = eachOf 10 (genTxWitness $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpAttributes
--------------------------------------------------------------------------------

roundTripUpAttributes :: Property
roundTripUpAttributes = eachOf 10 genUpAttributes roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateData
--------------------------------------------------------------------------------

roundTripUpdateData :: Property
roundTripUpdateData = eachOf 10 genUpdateData roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdatePayload
--------------------------------------------------------------------------------

roundTripUpdatePayload :: Property
roundTripUpdatePayload = eachOf 10 (feedPM genUpdatePayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProof
--------------------------------------------------------------------------------

roundTripUpdateProof :: Property
roundTripUpdateProof = eachOf 10 (genUpdateProof $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposal
--------------------------------------------------------------------------------

roundTripUpdateProposal :: Property
roundTripUpdateProposal = eachOf 10 (genUpdateProposal $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpdateProposals
--------------------------------------------------------------------------------

roundTripUpdateProposals :: Property
roundTripUpdateProposals = eachOf 10 (feedPM genUpdateProposals) roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateProposalToSign
--------------------------------------------------------------------------------

roundTripUpdateProposalToSign :: Property
roundTripUpdateProposalToSign = eachOf 10 genUpdateProposalToSign roundTripsBiShow

--------------------------------------------------------------------------------
-- UpdateVote
--------------------------------------------------------------------------------

roundTripUpdateVote :: Property
roundTripUpdateVote = eachOf 10 (genUpdateVote $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpId
--------------------------------------------------------------------------------

roundTripUpId :: Property
roundTripUpId = eachOf 10 (feedPM genUpId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData
--------------------------------------------------------------------------------

roundTripUpsData :: Property
roundTripUpsData = eachOf 10 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

roundTripVoteId :: Property
roundTripVoteId = eachOf 10 (feedPM genVoteId) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

roundTripVssCertificate :: Property
roundTripVssCertificate = eachOf 10 (genVssCertificate $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

roundTripVssCertificatesHash :: Property
roundTripVssCertificatesHash = eachOf 10 (feedPM genVssCertificatesHash) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

roundTripVssCertificatesMap :: Property
roundTripVssCertificatesMap = eachOf 10 (genVssCertificatesMap $ ProtocolMagic 0) roundTripsBiShow

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- TODO move this into util package? or crypto.gen?
feedPM :: (ProtocolMagic -> H.Gen a) -> H.Gen a
feedPM genA = genA =<< genProtocolMagic

feedPC :: (ProtocolConstants -> H.Gen a) -> H.Gen a
feedPC genA = genA =<< genProtocolConstants

feedPMC :: (ProtocolMagic -> ProtocolConstants -> H.Gen a) -> H.Gen a
feedPMC genA = do pm <- genProtocolMagic
                  pc <- genProtocolConstants
                  genA pm pc

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

hashTx :: Hash Tx
hashTx = hash $ UnsafeTx txInUtxoList txOutList (mkAttributes ())

txId :: TxId
txId = hashTx

txInUnknown :: TxIn
txInUnknown = TxInUnknown 47 ("forty seven" :: ByteString)

txInUtxo :: TxIn
txInUtxo = TxInUtxo hashTx 47


txInUtxoList :: (NonEmpty TxIn)
txInUtxoList = fromList [txInUtxo]

txInUnknownList :: (NonEmpty TxIn)
txInUnknownList = fromList [txInUnknown]

txOut :: TxOut
txOut = TxOut (makePubKeyAddress (IsBootstrapEraAddr True) pkey) (Coin 47)
    where
        Right pkey = PublicKey <$> xpub (getBytes 0 64)

txOutList :: (NonEmpty TxOut)
txOutList = fromList [txOut]

txSig :: TxSig
txSig = sign (ProtocolMagic 0) SignForTestingOnly skey txSigData
    where
        Right skey = SecretKey <$> xprv (getBytes 10 128)

txSigData :: TxSigData
txSigData = TxSigData hashTx

exampleSlotId :: SlotId
exampleSlotId = SlotId (EpochIndex 11) (UnsafeLocalSlotIndex 47)

exampleAddrSpendingData_PubKey :: AddrSpendingData
exampleAddrSpendingData_PubKey = PubKeyASD examplePubicKey

examplePubicKey :: PublicKey
examplePubicKey = pk
  where Right pk = PublicKey <$> xpub (getBytes 0 64)

exampleScript :: Script
exampleScript = Script 601 (getBytes 4 32)

exampleStakesList :: StakesList
exampleStakesList = zip sis coins
  where
    sis   = [si1, si2, si3]
    coins = map Coin [79, 44, 9999999]
    Right si1 = abstractHash . PublicKey <$> xpub (getBytes  0 64)
    Right si2 = abstractHash . PublicKey <$> xpub (getBytes 15 64)
    Right si3 = abstractHash . PublicKey <$> xpub (getBytes 30 64)

exampleSlotLeaders :: SlotLeaders
exampleSlotLeaders = map abstractHash (pk1 :| [pk2, pk3])
  where
    Right pk1 = PublicKey <$> xpub (getBytes  0 64)
    Right pk2 = PublicKey <$> xpub (getBytes 16 64)
    Right pk3 = PublicKey <$> xpub (getBytes 32 64)

-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkSequential $$discoverRoundTrip
