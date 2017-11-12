-- | This module tests Binary instances for Pos.Ssc types

module Test.Pos.Ssc.Identity.BinarySpec
       ( spec
       ) where

import           Universum

import           Data.Tagged (Tagged)
import           Test.Hspec (Spec, describe)

import           Pos.Arbitrary.Infra ()
import           Pos.Binary ()
import qualified Pos.Communication.Relay as R
import           Pos.Core (StakeholderId)
import qualified Pos.Core.Ssc as Ssc
import qualified Pos.Ssc as Ssc

import           Test.Pos.Helpers (binaryTest, msgLenLimitedTest)
import           Test.Pos.Util (withDefConfiguration, withDefInfraConfiguration)

spec :: Spec
spec = withDefInfraConfiguration $ withDefConfiguration $
  describe "Ssc" $ do
    describe "Bi instances" $ do
        binaryTest @Ssc.Commitment
        binaryTest @Ssc.CommitmentsMap
        binaryTest @Ssc.Opening
        binaryTest @Ssc.SscPayload
        binaryTest @Ssc.SscProof
        binaryTest @(R.InvMsg (Tagged Ssc.MCCommitment StakeholderId))
        binaryTest @(R.ReqMsg (Tagged Ssc.MCCommitment StakeholderId))
        binaryTest @(R.MempoolMsg Ssc.MCCommitment)
        binaryTest @(R.DataMsg Ssc.MCCommitment)
        binaryTest @(R.DataMsg Ssc.MCOpening)
        binaryTest @(R.DataMsg Ssc.MCShares)
        binaryTest @(R.DataMsg Ssc.MCVssCertificate)
        binaryTest @Ssc.SscTag
        binaryTest @Ssc.TossModifier
        binaryTest @Ssc.VssCertData
        binaryTest @Ssc.SscGlobalState
        binaryTest @Ssc.SscSecretStorage
    describe "Message length limit" $ do
        msgLenLimitedTest @Ssc.Opening
        msgLenLimitedTest @(R.InvMsg (Tagged Ssc.MCCommitment StakeholderId))
        msgLenLimitedTest @(R.ReqMsg (Tagged Ssc.MCCommitment StakeholderId))
        msgLenLimitedTest @(R.MempoolMsg Ssc.MCCommitment)
        -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg Ssc.MCCommitment))
        --     (C.MaxSize . R.DataMsg <$> C.mcCommitmentMsgLenLimit)
        --     "MCCommitment"
        --     (has Ssc._MCCommitment . R.dmContents . C.getOfMaxSize)
        -- msgLenLimitedTest' @(R.DataMsg Ssc.MCOpening)
        --     (R.DataMsg <$> C.mcOpeningLenLimit)
        --     "MCOpening"
        --     (has Ssc._MCOpening . R.dmContents)
        -- msgLenLimitedTest' @(C.MaxSize (R.DataMsg Ssc.MCShares))
        --     (C.MaxSize . R.DataMsg <$> C.mcSharesMsgLenLimit)
        --     "MCShares"
        --     (has Ssc._MCShares . R.dmContents . C.getOfMaxSize)
        -- msgLenLimitedTest' @(R.DataMsg Ssc.MCVssCertificate)
        --     (R.DataMsg <$> C.mcVssCertificateLenLimit)
        --     "MCVssCertificate"
        --     (has Ssc._MCVssCertificate . R.dmContents)
