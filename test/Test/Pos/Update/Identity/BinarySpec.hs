-- | This module tests Binary instances for Pos.Update types

module Test.Pos.Update.Identity.BinarySpec
       ( spec
       ) where

import           Test.Hspec       (Spec, describe)
import           Universum

import qualified Pos.Update.Types as U
import           Pos.Util.Relay   as R

import           Test.Pos.Util    (binaryTest)

spec :: Spec
spec = describe "Update system" $ do
    describe "Bi instances" $ do
        binaryTest @U.UpdateProposal
        binaryTest @U.UpdateVote
        binaryTest @U.UpdateData
        binaryTest @U.UpdatePayload
        binaryTest @U.SystemTag
        binaryTest @(R.InvMsg U.UpId U.ProposalMsgTag)
        binaryTest @(R.ReqMsg U.UpId U.ProposalMsgTag)
        binaryTest @(R.DataMsg U.UpId U.UpdateProposal)
