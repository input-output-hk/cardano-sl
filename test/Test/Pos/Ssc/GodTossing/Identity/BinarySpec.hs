-- | This module tests Binary instances for Pos.Ssc.GodTossing types

module Test.Pos.Ssc.GodTossing.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec         (Spec, describe)
import           Universum

import qualified Pos.Ssc.GodTossing as GT
import           Pos.Types.Address  (StakeholderId)
import           Pos.Util.Relay     as R

import           Test.Pos.Util      (binaryTest)

spec :: Spec
spec = describe "GodTossing" $ do
    describe "Bi instances" $ do
        binaryTest @GT.Commitment
        binaryTest @GT.Opening
        binaryTest @GT.VssCertificate
        binaryTest @GT.GtProof
        binaryTest @GT.GtPayload
        binaryTest @(R.InvMsg StakeholderId GT.GtMsgTag)
        binaryTest @(R.ReqMsg StakeholderId GT.GtMsgTag)
        binaryTest @(R.DataMsg StakeholderId GT.GtMsgContents)
        binaryTest @GT.GtSecretStorage
