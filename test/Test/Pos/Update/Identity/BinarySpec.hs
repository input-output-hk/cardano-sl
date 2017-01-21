-- | This module tests Binary instances for Pos.Update types

module Test.Pos.Update.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec     (Spec, describe)
import           Universum

import qualified Pos.Update     as U
import           Pos.Util.Relay as R

import           Test.Pos.Util  (binaryTest, networkBinaryTest)

spec :: Spec
spec = describe "Update system" $ do
    describe "Bi instances" $ do
        binaryTest @U.UpdateProposal
        binaryTest @U.UpdateVote
        binaryTest @U.UpdateData
        binaryTest @U.UpdatePayload
        binaryTest @U.SystemTag
        networkBinaryTest @(R.InvMsg U.UpId U.ProposalMsgTag)
        networkBinaryTest @(R.ReqMsg U.UpId U.ProposalMsgTag)
        networkBinaryTest @(R.DataMsg U.UpId (U.UpdateProposal, [U.UpdateVote]))
