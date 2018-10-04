module Test.Pos.Chain.Update.Gen
       ( genApplicationName
       , genBlockVersion
       , genBlockVersionData
       , genBlockVersionDataByTxFP
       , genBlockVersionModifier
       , genHashRaw
       , genSoftforkRule
       , genSoftwareVersion
       , genSystemTag
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

import qualified Data.HashMap.Strict as HM
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Chain.Update (ApplicationName (..), BlockVersion (..),
                     BlockVersionData (..), BlockVersionModifier (..),
                     SoftforkRule (..), SoftwareVersion (..), SystemTag (..),
                     UpAttributes, UpId, UpdateData (..), UpdatePayload (..),
                     UpdateProof, UpdateProposal (..),
                     UpdateProposalToSign (..), UpdateProposals,
                     UpdateVote (..), VoteId, mkUpdateVote)
import           Pos.Core (TxFeePolicy)
import           Pos.Core.Attributes (mkAttributes)
import           Pos.Crypto (ProtocolMagic)

import           Test.Pos.Core.Gen (genByte, genCoinPortion, genCustomHashMap,
                     genEpochIndex, genFlatSlotId, genHashRaw, genMillisecond,
                     genScriptVersion, genTxFeePolicy)
import           Test.Pos.Crypto.Gen (genAbstractHash, genPublicKey,
                     genSecretKey, genSignature)


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
genUpdateProposals pm = genCustomHashMap (genUpId pm) (genUpdateProposal pm)

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

genUpsData :: Gen (HM.HashMap SystemTag UpdateData)
genUpsData = do
    hMapSize <- Gen.int (Range.linear 0 20)
    sysTagList <- Gen.list (Range.singleton hMapSize) genSystemTag
    upDataList <- Gen.list (Range.singleton hMapSize) genUpdateData
    pure $ HM.fromList $ zip sysTagList upDataList

genUpdateVote :: ProtocolMagic -> Gen UpdateVote
genUpdateVote pm = mkUpdateVote pm <$> genSecretKey <*> genUpId pm <*> Gen.bool

genVoteId :: ProtocolMagic -> Gen VoteId
genVoteId pm = (,,) <$> genUpId pm <*> genPublicKey <*> Gen.bool
