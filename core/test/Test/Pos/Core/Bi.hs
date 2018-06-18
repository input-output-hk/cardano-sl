module Test.Pos.Core.Bi
    -- ( tests
    -- , roundTripAddressBi
    -- ) where
    where

import           Universum

import           Cardano.Crypto.Wallet (xprv, xpub)
import           Data.List.NonEmpty (fromList)
import           Data.Time.Units (fromMicroseconds)
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Core.Common (Coin (..), IsBootstrapEraAddr (..), Script (..),
                                  makePubKeyAddress)
import           Pos.Core.Slotting (Timestamp (..))
import           Pos.Core.Txp (Tx (..), TxId, TxIn (..), TxOutAux (..), TxInWitness (..),
                               TxOut (..),TxSig, TxSigData (..))
import           Pos.Crypto (Hash, ProtocolMagic (..), PublicKey (..), SecretKey (..),
                             hash, redeemDeterministicKeyGen, redeemSign, sign,
                             SignTag (..))
import           Pos.Data.Attributes (mkAttributes)


import           Test.Pos.Binary.Helpers.GoldenRoundTrip (discoverGolden, discoverRoundTrip, eachOf,
                                                          goldenTestBi, roundTripsBiBuildable,
                                                          roundTripsBiShow)
import           Test.Pos.Crypto.Gen (genProtocolMagic)
import           Test.Pos.Core.Gen
import           Test.Pos.Crypto.Bi (getBytes)

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
roundTripSscPayload = eachOf 10 (genSscPayload $ ProtocolMagic 0) roundTripsBiBuildable

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

roundTripTxAux :: Property
roundTripTxAux = eachOf 10 (genTxAux $ ProtocolMagic 0) roundTripsBiBuildable

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

roundTripTxPayload :: Property
roundTripTxPayload = eachOf 10 (genTxPayload $ ProtocolMagic 0) roundTripsBiShow

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
roundTripUpdatePayload = eachOf 10 (genUpdatePayload $ ProtocolMagic 0) roundTripsBiBuildable

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
roundTripUpdateProposals = eachOf 10 genUpdateProposals roundTripsBiShow

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
roundTripUpId = eachOf 10 genUpId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- UpsData
--------------------------------------------------------------------------------

roundTripUpsData :: Property
roundTripUpsData = eachOf 10 genUpsData roundTripsBiShow

--------------------------------------------------------------------------------
-- VoteId
--------------------------------------------------------------------------------

roundTripVoteId :: Property
roundTripVoteId = eachOf 10 genVoteId roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

roundTripVssCertificate :: Property
roundTripVssCertificate = eachOf 10 (genVssCertificate $ ProtocolMagic 0) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

roundTripVssCertificatesHash :: Property
roundTripVssCertificatesHash = eachOf 10 genVssCertificatesHash roundTripsBiBuildable

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


-----------------------------------------------------------------------
-- Main test export
-----------------------------------------------------------------------
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


--------------------------------------------------------------------------------

tests :: IO Bool
tests = (&&) <$> H.checkSequential $$discoverGolden
             <*> H.checkSequential $$discoverRoundTrip
