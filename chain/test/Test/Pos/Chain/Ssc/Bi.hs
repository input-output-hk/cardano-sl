{-# LANGUAGE TemplateHaskell #-}

module Test.Pos.Chain.Ssc.Bi
       ( tests
       ) where

import           Universum

import qualified Data.HashMap.Strict as HM
import           Hedgehog (Property)
import qualified Hedgehog as H

import           Pos.Chain.Ssc (SscPayload (..), SscProof (..))
import           Pos.Crypto (hash)

import           Test.Pos.Binary.Helpers.GoldenRoundTrip (goldenTestBi,
                     roundTripsBiBuildable, roundTripsBiShow)
import           Test.Pos.Chain.Ssc.Example (exampleCommitment,
                     exampleCommitmentSignature, exampleCommitmentsMap,
                     exampleInnerSharesMap, exampleOpening, exampleOpeningsMap,
                     exampleSharesDistribution, exampleSignedCommitment,
                     exampleSscPayload, exampleSscProof, exampleVssCertificate,
                     exampleVssCertificatesHash, exampleVssCertificatesMap)
import           Test.Pos.Chain.Ssc.Gen (genCommitment, genCommitmentSignature,
                     genCommitmentsMap, genInnerSharesMap, genOpening,
                     genOpeningsMap, genSharesDistribution, genSharesMap,
                     genSignedCommitment, genSscPayload, genSscProof,
                     genVssCertificate, genVssCertificatesHash,
                     genVssCertificatesMap)
import           Test.Pos.Core.ExampleHelpers (exampleStakeholderId, feedPM)
import           Test.Pos.Util.Golden (discoverGolden, eachOf)
import           Test.Pos.Util.Tripping (discoverRoundTrip)

--------------------------------------------------------------------------------
-- Commitment
--------------------------------------------------------------------------------

golden_Commitment :: Property
golden_Commitment = goldenTestBi exampleCommitment "test/golden/bi/ssc/Commitment"

roundTripCommitment :: Property
roundTripCommitment = eachOf 10 genCommitment roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsMap
--------------------------------------------------------------------------------

golden_CommitmentsMap :: Property
golden_CommitmentsMap =
  goldenTestBi exampleCommitmentsMap "test/golden/bi/ssc/CommitmentsMap"

roundTripCommitmentsMap :: Property
roundTripCommitmentsMap = eachOf 10 (feedPM genCommitmentsMap) roundTripsBiShow

--------------------------------------------------------------------------------
-- CommitmentsSignature
--------------------------------------------------------------------------------

golden_CommitmentSignature :: Property
golden_CommitmentSignature =
    goldenTestBi exampleCommitmentSignature "test/golden/bi/ssc/CommitmentSignature"

roundTripCommitmentSignature :: Property
roundTripCommitmentSignature = eachOf 10 (feedPM genCommitmentSignature) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- InnerSharesMap
--------------------------------------------------------------------------------

golden_InnerSharesMap :: Property
golden_InnerSharesMap = goldenTestBi iSm "test/golden/bi/ssc/InnerSharesMap"
    where iSm = exampleInnerSharesMap 3 1

roundTripInnerSharesMap :: Property
roundTripInnerSharesMap = eachOf 50 genInnerSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- Opening
--------------------------------------------------------------------------------

golden_Opening :: Property
golden_Opening = goldenTestBi exampleOpening "test/golden/bi/ssc/Opening"

roundTripOpening :: Property
roundTripOpening = eachOf 10 genOpening roundTripsBiBuildable

--------------------------------------------------------------------------------
-- OpeningsMap
--------------------------------------------------------------------------------

golden_OpeningsMap :: Property
golden_OpeningsMap = goldenTestBi exampleOpeningsMap "test/golden/bi/ssc/OpeningsMap"

roundTripOpeningsMap :: Property
roundTripOpeningsMap = eachOf 10 genOpeningsMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SignedCommitment
--------------------------------------------------------------------------------

golden_SignedCommitment :: Property
golden_SignedCommitment =
    goldenTestBi exampleSignedCommitment "test/golden/bi/ssc/SignedCommitment"

roundTripSignedCommitment :: Property
roundTripSignedCommitment =
    eachOf 10 (feedPM genSignedCommitment) roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesDistribution
--------------------------------------------------------------------------------

golden_SharesDistribution :: Property
golden_SharesDistribution =
    goldenTestBi exampleSharesDistribution "test/golden/bi/ssc/SharesDistribution"

roundTripSharesDistribution :: Property
roundTripSharesDistribution = eachOf 10 genSharesDistribution roundTripsBiShow

--------------------------------------------------------------------------------
-- SharesMap
--------------------------------------------------------------------------------

golden_SharesMap :: Property
golden_SharesMap = goldenTestBi sM "test/golden/bi/ssc/SharesMap"
    where
        sM = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

roundTripSharesMap :: Property
roundTripSharesMap = eachOf 10 genSharesMap roundTripsBiShow

--------------------------------------------------------------------------------
-- SscPayload
--------------------------------------------------------------------------------

golden_SscPayload_CommitmentsPayload :: Property
golden_SscPayload_CommitmentsPayload =
    goldenTestBi cP "test/golden/bi/ssc/SscPayload_CommitmentsPayload"
  where
    cP = CommitmentsPayload exampleCommitmentsMap (exampleVssCertificatesMap 10 4)

golden_SscPayload_OpeningsPayload :: Property
golden_SscPayload_OpeningsPayload =
    goldenTestBi oP "test/golden/bi/ssc/SscPayload_OpeningsPayload"
  where
    oP = OpeningsPayload exampleOpeningsMap (exampleVssCertificatesMap 10 4)


golden_SscPayload_SharesPayload :: Property
golden_SscPayload_SharesPayload =
    goldenTestBi exampleSscPayload "test/golden/bi/ssc/SscPayload_SharesPayload"

golden_SscPayload_CertificatesPayload :: Property
golden_SscPayload_CertificatesPayload =
    goldenTestBi shP "test/golden/bi/ssc/SscPayload_CertificatesPayload"
  where
    shP = CertificatesPayload (exampleVssCertificatesMap 10 4)


roundTripSscPayload :: Property
roundTripSscPayload = eachOf 10 (feedPM genSscPayload) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- SscProof
--------------------------------------------------------------------------------

golden_SscProof_CommitmentsProof :: Property
golden_SscProof_CommitmentsProof =
    goldenTestBi exampleSscProof "test/golden/bi/ssc/SscProof_CommitmentsProof"

golden_SscProof_OpeningsProof :: Property
golden_SscProof_OpeningsProof =
    goldenTestBi oP "test/golden/bi/ssc/SscProof_OpeningsProof"
  where
    oP = OpeningsProof (hash exampleOpeningsMap) (exampleVssCertificatesHash 10 4)


golden_SscProof_SharesProof :: Property
golden_SscProof_SharesProof =
    goldenTestBi sP "test/golden/bi/ssc/SscProof_SharesProof"
  where
    sP = SharesProof (hash exampleSharesMap) (exampleVssCertificatesHash 10 4)
    exampleSharesMap = HM.fromList $ [(exampleStakeholderId, exampleInnerSharesMap 3 1)]

golden_SscProof_CertificatesProof :: Property
golden_SscProof_CertificatesProof =
    goldenTestBi shP "test/golden/bi/ssc/SscProof_CertificatesProof"
  where
    shP = CertificatesProof (exampleVssCertificatesHash 10 4)

roundTripSscProof :: Property
roundTripSscProof = eachOf 10 (feedPM genSscProof) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificate
--------------------------------------------------------------------------------

golden_VssCertificate :: Property
golden_VssCertificate = goldenTestBi exampleVssCertificate "test/golden/bi/ssc/VssCertificate"

roundTripVssCertificate :: Property
roundTripVssCertificate = eachOf 10 (feedPM genVssCertificate) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesHash
--------------------------------------------------------------------------------

golden_VssCertificatesHash :: Property
golden_VssCertificatesHash = goldenTestBi (exampleVssCertificatesHash 10 4) "test/golden/bi/ssc/VssCertificatesHash"

roundTripVssCertificatesHash :: Property
roundTripVssCertificatesHash = eachOf 10 (feedPM genVssCertificatesHash) roundTripsBiBuildable

--------------------------------------------------------------------------------
-- VssCertificatesMap
--------------------------------------------------------------------------------

golden_VssCertificatesMap :: Property
golden_VssCertificatesMap = goldenTestBi (exampleVssCertificatesMap 10 4) "test/golden/bi/ssc/VssCertificatesMap"

roundTripVssCertificatesMap :: Property
roundTripVssCertificatesMap = eachOf 10 (feedPM genVssCertificatesMap) roundTripsBiShow

--------------------------------------------------------------------------------
-- Main test export
--------------------------------------------------------------------------------

tests :: IO Bool
tests = and <$> sequence
    [ H.checkSequential $$discoverGolden
    , H.checkParallel $$discoverRoundTrip
    ]
