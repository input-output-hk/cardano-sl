-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Data.Tagged             (Tagged)
import           Test.Hspec              (Spec, describe)

import           Pos.Arbitrary.Infra     ()
import           Pos.Binary              ()
import qualified Pos.Communication.Relay as R
import           Pos.Core                (StakeholderId)
import qualified Pos.Ssc.GodTossing      as GT
import           Test.Pos.Util           (binaryTest, giveInfraConf, giveCoreConf,
                                          msgLenLimitedTest)

spec :: Spec
spec = giveInfraConf $ giveCoreConf $ describe "GodTossing" $ do
    describe "Bi instances" $ do
        binaryTest @GT.Commitment
        binaryTest @GT.CommitmentsMap
        binaryTest @GT.Opening
        binaryTest @GT.GtPayload
        binaryTest @GT.GtProof
        binaryTest @(R.InvMsg (Tagged GT.MCCommitment StakeholderId))
        binaryTest @(R.ReqMsg (Tagged GT.MCCommitment StakeholderId))
        binaryTest @(R.MempoolMsg GT.MCCommitment)
        binaryTest @(R.DataMsg GT.MCCommitment)
        binaryTest @(R.DataMsg GT.MCOpening)
        binaryTest @(R.DataMsg GT.MCShares)
        binaryTest @(R.DataMsg GT.MCVssCertificate)
        binaryTest @GT.GtTag
        binaryTest @GT.TossModifier
        binaryTest @GT.VssCertData
        binaryTest @GT.GtGlobalState
        binaryTest @GT.GtSecretStorage
    describe "Message length limit" $ do
        msgLenLimitedTest @GT.Opening
        msgLenLimitedTest @(R.InvMsg (Tagged GT.MCCommitment StakeholderId))
        msgLenLimitedTest @(R.ReqMsg (Tagged GT.MCCommitment StakeholderId))
        msgLenLimitedTest @(R.MempoolMsg GT.MCCommitment)
        -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg GT.MCCommitment))
        --     (C.MaxSize . R.DataMsg <$> C.mcCommitmentMsgLenLimit)
        --     "MCCommitment"
        --     (has GT._MCCommitment . R.dmContents . C.getOfMaxSize)
        -- msgLenLimitedTest' @(R.DataMsg GT.MCOpening)
        --     (R.DataMsg <$> C.mcOpeningLenLimit)
        --     "MCOpening"
        --     (has GT._MCOpening . R.dmContents)
        -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg GT.MCShares))
        --     (C.MaxSize . R.DataMsg <$> C.mcSharesMsgLenLimit)
        --     "MCShares"
        --     (has GT._MCShares . R.dmContents . C.getOfMaxSize)
        -- msgLenLimitedTest' @(R.DataMsg GT.MCVssCertificate)
        --     (R.DataMsg <$> C.mcVssCertificateLenLimit)
        --     "MCVssCertificate"
        --     (has GT._MCVssCertificate . R.dmContents)
