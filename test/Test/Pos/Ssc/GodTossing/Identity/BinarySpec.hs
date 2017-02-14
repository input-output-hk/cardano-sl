-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Crypto.Hash             (Blake2s_224, Blake2s_256)
import           Test.Hspec              (Spec, describe)
import           Universum

import           Pos.Binary              ()
import qualified Pos.Constants           as Const
import qualified Pos.Communication       as C
import qualified Pos.Communication.Relay as R
import           Pos.Crypto              (Signature, PublicKey,
                                          SecretSharingExtra (..), SecretProof,
                                          VssPublicKey, EncShare, AbstractHash,
                                          Share)
import qualified Pos.Ssc.GodTossing      as GT
import           Pos.Types.Address       (StakeholderId)
import           Test.Pos.Util           (binaryTest, msgLenLimitedTest,
                                          msgLenLimitedTest')

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
        binaryTest @(R.DataMsg GT.GtMsgContents)
        binaryTest @GT.GtSecretStorage
    describe "Message length limit" $ do
        -- TODO: move somewhere
        msgLenLimitedTest @PublicKey
        msgLenLimitedTest @EncShare
        msgLenLimitedTest @SecretSharingExtra
        msgLenLimitedTest @(C.MaxSize SecretSharingExtra)
        msgLenLimitedTest @(Signature ())
        msgLenLimitedTest @(AbstractHash Blake2s_224 Void)
        msgLenLimitedTest @(AbstractHash Blake2s_256 Void)
        msgLenLimitedTest @SecretProof
        msgLenLimitedTest @Share
        msgLenLimitedTest @VssPublicKey

        msgLenLimitedTest @(R.InvMsg StakeholderId GT.GtTag)
        msgLenLimitedTest @(R.ReqMsg StakeholderId GT.GtTag)
        msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
            (R.DataMsg <$> C.mcCommitmentMsgLenLimit)
            "MCCommitment"
            isMCCommitment
        msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
            (fromIntegral Const.genesisMaxMCOpeningSize)
            "MCOpening"
            isMCOpening
        msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
            (R.DataMsg <$> C.mcSharesMsgLenLimit)
            "MCShares"
            isMCShares
        msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
            (fromIntegral Const.genesisMaxMCVssCertificateSize)
            "MCVssCertificate"
            isMCVssCertificate


isMCCommitment :: R.DataMsg GT.GtMsgContents -> Bool
isMCCommitment (R.DataMsg (GT.MCCommitment _)) = True
isMCCommitment _                               = False

isMCOpening :: R.DataMsg GT.GtMsgContents -> Bool
isMCOpening (R.DataMsg (GT.MCOpening _ _)) = True
isMCOpening _                              = False

isMCShares :: R.DataMsg GT.GtMsgContents -> Bool
isMCShares (R.DataMsg (GT.MCShares _ _)) = True
isMCShares _                             = False

isMCVssCertificate :: R.DataMsg GT.GtMsgContents -> Bool
isMCVssCertificate (R.DataMsg (GT.MCVssCertificate _)) = True
isMCVssCertificate _                                   = False
