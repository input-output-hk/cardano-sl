-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Crypto.Hash                            (Blake2b_224, Blake2b_256)
import           Test.Hspec                             (Spec, describe)

import           Pos.Binary                             ()
import qualified Pos.Communication.Relay                as R
import           Pos.Core                               (StakeholderId)
import           Pos.Crypto                             (AbstractHash, EncShare,
                                                         PublicKey, SecretProof, Share,
                                                         Signature, VssPublicKey)
import qualified Pos.Ssc.GodTossing                     as GT
import           Test.Pos.Arbitrary.Infra.Communication ()
import           Test.Pos.Util                          (binaryTest, msgLenLimitedTest)

spec :: Spec
spec = describe "GodTossing" $ do
    describe "Bi instances" $ do
        binaryTest @GT.Commitment
        binaryTest @GT.CommitmentsMap
        binaryTest @GT.Opening
        binaryTest @GT.VssCertificate
        binaryTest @GT.GtProof
        binaryTest @GT.GtPayload
        binaryTest @(R.InvMsg StakeholderId GT.GtTag)
        binaryTest @(R.ReqMsg StakeholderId GT.GtTag)
        binaryTest @(R.MempoolMsg GT.GtTag)
        binaryTest @(R.DataMsg GT.GtMsgContents)
        binaryTest @GT.GtSecretStorage
    describe "Message length limit" $ do
        -- TODO: move somewhere (these types are not from GodTossing)
        msgLenLimitedTest @PublicKey
        msgLenLimitedTest @EncShare
        -- msgLenLimitedTest @(C.MaxSize SecretSharingExtra)
        msgLenLimitedTest @(Signature ())
        msgLenLimitedTest @(AbstractHash Blake2b_224 Void)
        msgLenLimitedTest @(AbstractHash Blake2b_256 Void)
        msgLenLimitedTest @SecretProof
        msgLenLimitedTest @VssPublicKey
        msgLenLimitedTest @Share

        msgLenLimitedTest @GT.Opening
        msgLenLimitedTest @GT.VssCertificate

        msgLenLimitedTest @(R.InvMsg StakeholderId GT.GtTag)
        msgLenLimitedTest @(R.ReqMsg StakeholderId GT.GtTag)
        msgLenLimitedTest @(R.MempoolMsg GT.GtTag)
        -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg GT.GtMsgContents))
        --     (C.MaxSize . R.DataMsg <$> C.mcCommitmentMsgLenLimit)
        --     "MCCommitment"
        --     (has GT._MCCommitment . R.dmContents . C.getOfMaxSize)
        -- msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
        --     (R.DataMsg <$> C.mcOpeningLenLimit)
        --     "MCOpening"
        --     (has GT._MCOpening . R.dmContents)
        -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg GT.GtMsgContents))
        --     (C.MaxSize . R.DataMsg <$> C.mcSharesMsgLenLimit)
        --     "MCShares"
        --     (has GT._MCShares . R.dmContents . C.getOfMaxSize)
        -- msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
        --     (R.DataMsg <$> C.mcVssCertificateLenLimit)
        --     "MCVssCertificate"
        --     (has GT._MCVssCertificate . R.dmContents)
