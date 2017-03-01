-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Control.Lens            (has)
import           Crypto.Hash             (Blake2s_224, Blake2s_256)
import           Test.Hspec              (Spec, describe)
import           Universum

import           Pos.Binary              ()
import qualified Pos.Communication       as C
import qualified Pos.Communication.Relay as R
import           Pos.Crypto              (AbstractHash, EncShare, PublicKey, SecretProof,
                                          SecretSharingExtra (..), Share, Signature,
                                          VssPublicKey)
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
        msgLenLimitedTest @(C.MaxSize SecretSharingExtra)
        msgLenLimitedTest @(Signature ())
        msgLenLimitedTest @(AbstractHash Blake2s_224 Void)
        msgLenLimitedTest @(AbstractHash Blake2s_256 Void)
        msgLenLimitedTest @SecretProof
        msgLenLimitedTest @Share
        msgLenLimitedTest @VssPublicKey

        msgLenLimitedTest @(R.InvMsg StakeholderId GT.GtTag)
        msgLenLimitedTest @(R.ReqMsg StakeholderId GT.GtTag)
        msgLenLimitedTest' @(C.MaxSize (R.DataMsg GT.GtMsgContents))
            (C.MaxSize . R.DataMsg <$> C.mcCommitmentMsgLenLimit)
            "MCCommitment"
            (has GT._MCCommitment . R.dmContents . C.getOfMaxSize)
        msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
            (R.DataMsg <$> C.mcOpeningLenLimit)
            "MCOpening"
            (has GT._MCOpening . R.dmContents)
        msgLenLimitedTest' @(C.MaxSize (R.DataMsg GT.GtMsgContents))
            (C.MaxSize . R.DataMsg <$> C.mcSharesMsgLenLimit)
            "MCShares"
            (has GT._MCShares . R.dmContents . C.getOfMaxSize)
        msgLenLimitedTest' @(R.DataMsg GT.GtMsgContents)
            (R.DataMsg <$> C.mcVssCertificateLenLimit)
            "MCVssCertificate"
            (has GT._MCVssCertificate . R.dmContents)
