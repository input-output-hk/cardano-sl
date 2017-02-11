-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec              (Spec, describe)
import           Universum

import           Pos.Binary              ()
import qualified Pos.Constants           as Const
import qualified Pos.Communication.Relay as R
import qualified Pos.Ssc.GodTossing      as GT
import           Pos.Types.Address       (StakeholderId)

import           Test.Pos.Util           (binaryTest, msgLenLimitedTest)

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
        msgLenLimitedTest
            @(R.InvMsg StakeholderId GT.GtTag) Const.genesisMaxInvSize
        msgLenLimitedTest
            @(R.ReqMsg StakeholderId GT.GtTag) Const.genesisMaxReqSize
