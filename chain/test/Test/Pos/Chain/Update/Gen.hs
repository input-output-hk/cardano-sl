module Test.Pos.Chain.Update.Gen
       ( genUpdateConfiguration
       , genApplicationName
       , genBlockVersion
       , genBlockVersionData
       , genBlockVersionDataByTxFP
       , genBlockVersionModifier
       , genHashRaw
       , genSoftforkRule
       , genSoftwareVersion
       , genSystemTag
       , genUndo
       , genUpAttributes
       , genUpdateData
       , genUpdatePayload
       , genUpdateProof
       , genUpdateProposal
       , genUpdateProposals
       , genUpdateProposalToSign
       , genUpdateVote
       , genUpId
       , genUpsData
       , genVoteId
       ) where

import           Universum

import           Data.Coerce (coerce)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Block (HeaderHash)
import           Pos.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     BlockVersionState (..), ConfirmedProposalState (..),
                     DecidedProposalState (..), DpsExtra (..), PrevValue,
                     ProposalState (..), SoftforkRule (..),
                     SoftwareVersion (..), SystemTag (..), USUndo (..),
                     UndecidedProposalState (..), UpAttributes, UpId,
                     UpdateConfiguration (..), UpdateData (..),
                     UpdatePayload (..), UpdateProof, UpdateProposal (..),
                     UpdateProposalToSign (..), UpdateProposals,
                     UpdateVote (..), UpsExtra (..), VoteId, VoteState (..),
                     maybeToPrev, mkUpdateVote)
import           Pos.Core (SlotCount, TxFeePolicy)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Core.Gen (genByte, genChainDifficulty, genCoin,
                     genCoinPortion, genEpochIndex, genFlatSlotId, genHashRaw,
                     genMillisecond, genScriptVersion, genSlotId,
                     genSlottingData, genStakeholderId, genTextHash,
                     genTxFeePolicy)
import           Test.Pos.Crypto.Gen (genAbstractHash, genPublicKey,
                     genSecretKey, genSignature)
import           Test.Pos.Util.Gen (genHashMap, genHashSet)


genUpdateConfiguration :: Gen UpdateConfiguration
genUpdateConfiguration = UpdateConfiguration <$> genApplicationName <*> genBlockVersion <*> (pure 1) <*> genSystemTag

genApplicationName :: Gen ApplicationName
genApplicationName =
    ApplicationName <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genBlockVersion :: Gen BlockVersion
genBlockVersion =
    BlockVersion
        <$> Gen.word16 Range.constantBounded
        <*> Gen.word16 Range.constantBounded
        <*> Gen.word8 Range.constantBounded

genBlockVersionData :: Gen BlockVersionData
genBlockVersionData = genBlockVersionDataByTxFP genTxFeePolicy

genBlockVersionDataByTxFP :: Gen TxFeePolicy -> Gen BlockVersionData
genBlockVersionDataByTxFP genTxFP =
    BlockVersionData
        <$> genScriptVersion
        <*> genMillisecond
        <*> genByte
        <*> genByte
        <*> genByte
        <*> genByte
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genCoinPortion
        <*> genFlatSlotId
        <*> genSoftforkRule
        <*> genTxFP
        <*> genEpochIndex

genBlockVersionModifier :: Gen BlockVersionModifier
genBlockVersionModifier =
    BlockVersionModifier
        <$> Gen.maybe genScriptVersion
        <*> Gen.maybe genMillisecond
        <*> Gen.maybe genByte
        <*> Gen.maybe genByte
        <*> Gen.maybe genByte
        <*> Gen.maybe genByte
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genCoinPortion
        <*> Gen.maybe genFlatSlotId
        <*> Gen.maybe genSoftforkRule
        <*> Gen.maybe genTxFeePolicy
        <*> Gen.maybe genEpochIndex

genBlockVersionState :: Gen BlockVersionState
genBlockVersionState = BlockVersionState
    <$> genBlockVersionModifier
    <*> Gen.maybe genEpochIndex
    <*> genHashSet genStakeholderId
    <*> genHashSet genStakeholderId
    <*> Gen.maybe genHeaderHash
    <*> Gen.maybe genHeaderHash

genConfirmedProposalState :: ProtocolMagic -> Gen ConfirmedProposalState
genConfirmedProposalState pm = ConfirmedProposalState
    <$> genUpdateProposal pm
    <*> Gen.bool
    <*> genHeaderHash
    <*> genHeaderHash
    <*> genHeaderHash
    <*> Gen.maybe genHeaderHash
    <*> genHashMap (Range.linear 1 10) genPublicKey genVoteState
    <*> genCoin
    <*> genCoin

genDecidedProposalState :: ProtocolMagic -> SlotCount -> Gen DecidedProposalState
genDecidedProposalState pm epochSlots = DecidedProposalState
    <$> Gen.bool
    <*> genUndecidedProposalState pm epochSlots
    <*> Gen.maybe genChainDifficulty
    <*> Gen.maybe genDpsExtra

genDpsExtra :: Gen DpsExtra
genDpsExtra = DpsExtra <$> genHeaderHash <*> Gen.bool

genPrevValue :: Gen a -> Gen (PrevValue a)
genPrevValue = fmap maybeToPrev . Gen.maybe

genProposalState :: ProtocolMagic -> SlotCount -> Gen ProposalState
genProposalState pm epochSlots = Gen.choice
  [ PSUndecided <$> genUndecidedProposalState pm epochSlots
  , PSDecided <$> genDecidedProposalState pm epochSlots
  ]

genSoftforkRule :: Gen SoftforkRule
genSoftforkRule =
    SoftforkRule <$> genCoinPortion <*> genCoinPortion <*> genCoinPortion

genSoftwareVersion :: Gen SoftwareVersion
genSoftwareVersion =
    SoftwareVersion
        <$> genApplicationName
        <*> Gen.word32 Range.constantBounded

genSystemTag :: Gen SystemTag
genSystemTag = SystemTag <$> Gen.text (Range.constant 0 10) Gen.alphaNum

genUndecidedProposalState
  :: ProtocolMagic -> SlotCount -> Gen UndecidedProposalState
genUndecidedProposalState pm epochSlots = UndecidedProposalState
    <$> genHashMap (Range.linear 0 10) genPublicKey genVoteState
    <*> genUpdateProposal pm
    <*> genSlotId epochSlots
    <*> genCoin
    <*> genCoin
    <*> Gen.maybe genUpsExtra

genUndo :: ProtocolMagic -> SlotCount -> Gen USUndo
genUndo pm epochSlots = USUndo
    <$> genHashMap hmRange genBlockVersion (genPrevValue genBlockVersionState)
    <*> Gen.maybe genBlockVersion
    <*> genHashMap
          hmRange
          (genUpId pm)
          (genPrevValue $ genProposalState pm epochSlots)
    <*> genHashMap
          hmRange
          genApplicationName
          (genPrevValue (Gen.word32 Range.constantBounded))
    <*> genHashMap
          hmRange
          genSoftwareVersion
          (genPrevValue $ genConfirmedProposalState pm)
    <*> Gen.maybe (genHashSet genStakeholderId)
    <*> Gen.maybe genSlottingData
  where hmRange = Range.linear 0 10

genUpAttributes :: Gen UpAttributes
genUpAttributes = pure $ mkAttributes ()

genUpdateData :: Gen UpdateData
genUpdateData =
    UpdateData
        <$> genHashRaw
        <*> genHashRaw
        <*> genHashRaw
        <*> genHashRaw

genUpdatePayload :: ProtocolMagic -> Gen UpdatePayload
genUpdatePayload pm =
    UpdatePayload
        <$> Gen.maybe (genUpdateProposal pm)
        <*> Gen.list (Range.linear 0 10) (genUpdateVote pm)

genUpdateProof :: ProtocolMagic -> Gen UpdateProof
genUpdateProof pm = genAbstractHash (genUpdatePayload pm)

genUpdateProposal :: ProtocolMagic -> Gen UpdateProposal
genUpdateProposal pm = do
    UnsafeUpdateProposal
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> genUpAttributes
        <*> genPublicKey
        <*> genSignature pm genUpdateProposalToSign

genUpdateProposals :: ProtocolMagic -> Gen UpdateProposals
genUpdateProposals pm =
  genHashMap (Range.linear 0 10) (genUpId pm) (genUpdateProposal pm)

genUpdateProposalToSign :: Gen UpdateProposalToSign
genUpdateProposalToSign =
    UpdateProposalToSign
        <$> genBlockVersion
        <*> genBlockVersionModifier
        <*> genSoftwareVersion
        <*> genUpsData
        <*> genUpAttributes

genUpId :: ProtocolMagic -> Gen UpId
genUpId pm = genAbstractHash (genUpdateProposal pm)

genUpsData :: Gen (HashMap SystemTag UpdateData)
genUpsData = genHashMap (Range.linear 0 20) genSystemTag genUpdateData

genUpsExtra :: Gen UpsExtra
genUpsExtra = UpsExtra <$> genHeaderHash

genUpdateVote :: ProtocolMagic -> Gen UpdateVote
genUpdateVote pm = mkUpdateVote pm <$> genSecretKey <*> genUpId pm <*> Gen.bool

genVoteId :: ProtocolMagic -> Gen VoteId
genVoteId pm = (,,) <$> genUpId pm <*> genPublicKey <*> Gen.bool

genVoteState :: Gen VoteState
genVoteState =
    Gen.element [PositiveVote, NegativeVote, PositiveRevote, NegativeRevote]

-- | Copied here from "Test.Pos.Chain.Block.Gen" to avoid module cycles
genHeaderHash :: Gen HeaderHash
genHeaderHash = coerce <$> genTextHash
