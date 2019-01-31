module Test.Pos.Chain.Ssc.Gen
       ( genSscConfiguration
       , genAttackTarget
       , genCommitment
       , genCommitmentsMap
       , genCommitmentSignature
       , genInnerSharesMap
       , genSharesMap
       , genOpening
       , genOpeningsMap
       , genSscPayload
       , genSscProof
       , genSharesDistribution
       , genSignedCommitment
       , genVssCertificate
       , genVssCertificatesHash
       , genVssCertificatesMap
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BC
import qualified Data.HashMap.Strict as HM
import           Data.List.NonEmpty (fromList)

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import           Pos.Binary.Class (asBinary)
import           Pos.Chain.Security (AttackTarget (..))
import           Pos.Chain.Ssc (Commitment, CommitmentSignature, CommitmentsMap,
                     InnerSharesMap, Opening, OpeningsMap, SharesDistribution,
                     SharesMap, SignedCommitment, SscConfiguration (..),
                     SscPayload (..), SscProof, VssCertificate (..),
                     VssCertificatesHash, VssCertificatesMap (..),
                     mkCommitmentsMap, mkSscProof, mkVssCertificate,
                     mkVssCertificatesMap, randCommitmentAndOpening)
import           Pos.Crypto (ProtocolMagic, deterministic, hash)

import           Test.Pos.Core.Gen (genEpochIndex, genStakeholderId, genWord16)
import           Test.Pos.Crypto.Gen (genDecShare, genPublicKey, genSecretKey,
                     genSignature, genVssPublicKey)
import           Test.Pos.Util.Gen (genHashMap)

genSscConfiguration :: Gen SscConfiguration
genSscConfiguration = SscConfiguration <$> (pure 10) <*> (pure 3) <*> (pure False)

genCommitment :: Gen Commitment
genCommitment = fst <$> genCommitmentOpening

genCommitmentOpening :: Gen (Commitment, Opening)
genCommitmentOpening = do
    let numKeys = 128 :: Int
    parties <-
        Gen.integral (Range.constant 4 (fromIntegral numKeys)) :: Gen Integer
    threshold <- Gen.integral (Range.constant 2 (parties - 2)) :: Gen Integer
    vssKeys <- replicateM numKeys genVssPublicKey
    pure
        $ deterministic "commitmentOpening"
        $ randCommitmentAndOpening threshold (fromList vssKeys)

genCommitmentSignature :: ProtocolMagic -> Gen CommitmentSignature
genCommitmentSignature pm = genSignature pm $ (,) <$> genEpochIndex <*> genCommitment

genCommitmentsMap :: ProtocolMagic -> Gen CommitmentsMap
genCommitmentsMap pm = mkCommitmentsMap <$> Gen.list range (genSignedCommitment pm)
  where
    range = Range.linear 0 10

genInnerSharesMap :: Gen InnerSharesMap
genInnerSharesMap = do
    hMS <- Gen.int (Range.linear 0 10)
    stakeholderId <- Gen.list (Range.singleton hMS) genStakeholderId
    nonEmptyDS <- Gen.nonEmpty (Range.singleton hMS) (asBinary <$> genDecShare)
    pure $ HM.fromList $ zip stakeholderId [nonEmptyDS]

genOpening :: Gen Opening
genOpening = snd <$> genCommitmentOpening

genOpeningsMap :: Gen OpeningsMap
genOpeningsMap = do
    hMapSize <- Gen.int (Range.linear 0 10)
    stakeholderId <- Gen.list (Range.singleton hMapSize) genStakeholderId
    opening <- Gen.list (Range.singleton hMapSize) genOpening
    pure $ HM.fromList $ zip stakeholderId opening

genSharesDistribution :: Gen SharesDistribution
genSharesDistribution =
    genHashMap (Range.linear 1 10) genStakeholderId genWord16

genSharesMap :: Gen SharesMap
genSharesMap = do
    hMapSize <- Gen.int (Range.linear 0 10)
    stakeholderId <- Gen.list (Range.singleton hMapSize) genStakeholderId
    innerSharesMap <- Gen.list (Range.singleton hMapSize) genInnerSharesMap
    pure $ HM.fromList $ zip stakeholderId innerSharesMap

genSignedCommitment :: ProtocolMagic -> Gen SignedCommitment
genSignedCommitment pm =
    (,,) <$> genPublicKey <*> genCommitment <*> genCommitmentSignature pm

-- We mod the size to the range [0,5000) to give relatively large tests which
-- are still reasonably fast to generate.
genSscPayload :: ProtocolMagic -> Gen SscPayload
genSscPayload pm = Gen.scale (`mod` 5000) $
    Gen.choice
        [ CertificatesPayload <$> genVssCertificatesMap pm
        , CommitmentsPayload <$> genCommitmentsMap pm <*> genVssCertificatesMap pm
        , OpeningsPayload <$> genOpeningsMap <*> genVssCertificatesMap pm
        , SharesPayload <$> genSharesMap <*> genVssCertificatesMap pm
        ]

genSscProof :: ProtocolMagic -> Gen SscProof
genSscProof pm = mkSscProof <$> genSscPayload pm

genVssCertificate :: ProtocolMagic -> Gen VssCertificate
genVssCertificate pm =
    mkVssCertificate pm
        <$> genSecretKey
        <*> (asBinary <$> genVssPublicKey)
        <*> genEpochIndex

genVssCertificatesHash :: ProtocolMagic -> Gen VssCertificatesHash
genVssCertificatesHash pm = hash
    <$> genHashMap (Range.linear 1 10) genStakeholderId (genVssCertificate pm)

genVssCertificatesMap :: ProtocolMagic -> Gen VssCertificatesMap
genVssCertificatesMap pm =
    mkVssCertificatesMap <$> Gen.list (Range.linear 0 5) (genVssCertificate pm)

----------------------------------------------------------------------------
-- Pos.Chain.Ssc.Security Generators
----------------------------------------------------------------------------

genAttackTarget :: Gen AttackTarget
genAttackTarget = do
    netHostAddr <- genHost
    port <- Gen.word16 (Range.constant 1 65535)
    Gen.choice [ pure $ NetworkAddressTarget (netHostAddr, port)
               , PubKeyAddressTarget <$> genStakeholderId
               ]

genHost :: Gen ByteString
genHost = BC.pack <$> Gen.string (Range.constant 1 10) Gen.alphaNum
