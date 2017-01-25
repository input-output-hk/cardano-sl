-- | This module tests Binary instances for Pos.Update types

module Test.Pos.Update.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec     (Spec, describe)
import           Universum

import qualified Pos.Update     as U
import           Pos.Util.Relay as R

import           Pos.Binary     ()
import           Test.Pos.Util  (binaryTest, networkBinaryTest)

spec :: Spec
spec =
  describe "Update system" $ do
    describe "Bi instances" $ do
      describe "Core" $ do
        binaryTest @U.UpId
        binaryTest @U.UpdateProposal
        binaryTest @U.UpdateVote
        binaryTest @U.UpdateData
        binaryTest @U.UpdatePayload
        binaryTest @U.SystemTag
      describe "Poll" $ do
        binaryTest @U.UndecidedProposalState
        binaryTest @U.UpsExtra
        binaryTest @U.DecidedProposalState
        binaryTest @U.DpsExtra
        binaryTest @U.ConfirmedProposalState
        -- TODO: binaryTest @U.ProposalState
      describe "Network" $ do
        networkBinaryTest @(R.InvMsg U.VoteId U.VoteMsgTag)
        networkBinaryTest @(R.ReqMsg U.VoteId U.VoteMsgTag)
        networkBinaryTest @(R.DataMsg U.VoteId U.UpdateVote)
        networkBinaryTest @(R.InvMsg U.UpId U.ProposalMsgTag)
        networkBinaryTest @(R.ReqMsg U.UpId U.ProposalMsgTag)
        -- networkBinaryTest @(R.DataMsg U.UpId (U.UpdateProposal, [U.UpdateVote]))
