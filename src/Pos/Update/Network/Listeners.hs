{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usListeners
       , usStubListeners
       ) where

import           Formatting                 (build, sformat, (%))
import           Serokell.Util.Verify       (VerificationRes (..))
import           System.Wlog                (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Binary.Relay           ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (ListenerSpec, OutSpecs)
import           Pos.Communication.Relay    (Relay (..), RelayProxy (..), relayListeners,
                                             relayStubListeners)
import           Pos.Update.Core            (UpId, UpdateProposal (..), UpdateVote (..),
                                             VoteId)
import           Pos.Update.Logic.Local     (getLocalProposalNVotes, getLocalVote,
                                             isProposalNeeded, isVoteNeeded,
                                             processProposal, processVote)
import           Pos.Update.Network.Types   (ProposalMsgTag (..), VoteMsgTag (..))
import           Pos.Util                   (mappendPair)
import           Pos.WorkMode               (WorkMode)

proposalProxy :: RelayProxy UpId ProposalMsgTag (UpdateProposal, [UpdateVote])
proposalProxy = RelayProxy

voteProxy :: RelayProxy VoteId VoteMsgTag UpdateVote
voteProxy = RelayProxy

-- | Listeners for requests related to update system
usListeners
    :: (WorkMode ssc m)
    => ([ListenerSpec m], OutSpecs)
usListeners = mappendPair
                (relayListeners proposalProxy)
                (relayListeners voteProxy)

usStubListeners
    :: (WithLogger m)
    => ([ListenerSpec m], OutSpecs)
usStubListeners = mappendPair
                (relayStubListeners proposalProxy)
                (relayStubListeners voteProxy)

----------------------------------------------------------------------------
-- UpdateProposal listeners
----------------------------------------------------------------------------

instance WorkMode ssc m =>
         Relay m ProposalMsgTag UpId (UpdateProposal, [UpdateVote]) where
    contentsToTag _ = pure ProposalMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ = isProposalNeeded
    handleReq _ = getLocalProposalNVotes
    handleData (proposal, votes) _ = do
        res <- processProposal proposal
        logProp res
        let processed = isRight res
        processed <$ when processed (mapM_ processVoteLog votes)
      where
        processVoteLog = processVote >=> logVote
        logVote e@(Left cause) =
            e <$ logWarning (sformat ("Proposal accepted but vote "%build%" rejected") cause)
        logVote e@(Right _) =
            e <$ logDebug "Processing of proposal's vote is successfull"

        logProp (Left cause) =
            logDebug $ sformat ("Processing of proposal failed: "%build) cause
        logProp (Right _) = logDebug "Processing of proposal is successfull"

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

instance WorkMode ssc m =>
         Relay m VoteMsgTag VoteId UpdateVote where
    contentsToTag _ = pure VoteMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents UpdateVote{..} = pure VerSuccess

    handleInv _ (id, pk, dec) = isVoteNeeded id pk dec
    handleReq _ (id, pk, dec) = getLocalVote id pk dec
    handleData uv _ = do
        res <- processVote uv
        logProcess res
        pure $ isRight res
      where
        logProcess (Left cause) =
          logDebug $ sformat ("Processing of vote failed: "%build) cause
        logProcess (Right _) = logDebug $ "Processing of vote is successfull"
