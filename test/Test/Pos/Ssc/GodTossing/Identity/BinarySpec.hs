-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec               (Spec, describe)
import           Universum

import           Pos.Communication.Relay  as R
import           Pos.Core.Types           (StakeholderId)
import qualified Pos.Ssc.GodTossing       as GT

import           Test.Pos.Arbitrary.Infra ()
import           Test.Pos.Util            (binaryTest)

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
