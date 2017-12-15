-- | This module tests Binary instances for Pos.Update types

module Test.Pos.Update.Identity.BinarySpec
       ( spec
       ) where

import           Data.Tagged (Tagged)
import           Test.Hspec (Spec, describe)
import           Universum

import           Pos.Arbitrary.Infra ()
import           Pos.Binary ()
import           Pos.Communication.Relay ()
import qualified Pos.Communication.Relay as R
import qualified Pos.Update as U

import           Test.Pos.Cbor.CborSpec (U)
import           Test.Pos.Helpers (binaryTest, msgLenLimitedTest)
import           Test.Pos.Util (withDefConfiguration, withDefInfraConfiguration)

type VoteId' = Tagged U.UpdateVote U.VoteId
type UpId' = Tagged (U.UpdateProposal, [U.UpdateVote])U.UpId

spec :: Spec
spec = withDefInfraConfiguration $ withDefConfiguration $
    describe "Update system" $ do
        describe "Bi instances" $ do
            describe "Core" $ do
                binaryTest @U.BlockVersionModifier
                binaryTest @U.SystemTag
                binaryTest @U.UpdateVote
                binaryTest @U.UpdateData
                binaryTest @U.UpdateProposal
                binaryTest @U.UpdateProposalToSign
                binaryTest @U.UpdatePayload
                binaryTest @U.VoteState
                binaryTest @U.UpId
            describe "Poll" $ do
                binaryTest @(U.PrevValue ())
                binaryTest @(U.PrevValue U)
                binaryTest @U.USUndo
                binaryTest @U.UpsExtra
                binaryTest @U.DpsExtra
                binaryTest @U.UndecidedProposalState
                binaryTest @U.DecidedProposalState
                binaryTest @U.ProposalState
                binaryTest @U.ConfirmedProposalState
                binaryTest @U.BlockVersionState
            describe "Network" $ do
                binaryTest @(R.InvMsg VoteId')
                binaryTest @(R.ReqMsg VoteId')
                binaryTest @(R.MempoolMsg U.UpdateVote)
                binaryTest @(R.DataMsg U.UpdateVote)
                binaryTest @(R.InvMsg UpId')
                binaryTest @(R.ReqMsg UpId')
                binaryTest @(R.MempoolMsg (U.UpdateProposal, [U.UpdateVote]))
                binaryTest @(R.DataMsg (U.UpdateProposal, [U.UpdateVote]))
            describe "Message length limit" $ do
                msgLenLimitedTest @(R.InvMsg VoteId')
                msgLenLimitedTest @(R.ReqMsg VoteId')
                msgLenLimitedTest @(R.MempoolMsg U.UpdateVote)
                msgLenLimitedTest @(R.InvMsg UpId')
                msgLenLimitedTest @(R.ReqMsg UpId')
                msgLenLimitedTest @(R.MempoolMsg (U.UpdateProposal, [U.UpdateVote]))
                -- TODO [CSL-859]
                -- msgLenLimitedTest @(C.MaxSize (R.DataMsg (U.UpdateProposal, [U.UpdateVote])))
                msgLenLimitedTest @(R.DataMsg U.UpdateVote)
                -- msgLenLimitedTest @U.UpdateProposal
