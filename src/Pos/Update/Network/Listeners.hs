{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usListeners
       ) where

import           Node                     (ListenerAction (..))
import           Serokell.Util.Verify     (VerificationRes (..))
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Relay         ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Update.Core          (UpId, UpdateProposal (..), UpdateVote (..),
                                           VoteId)
import           Pos.Update.Logic.Local   (getLocalProposal, getLocalVote,
                                           isProposalNeeded, isVoteNeeded,
                                           processProposal, processVote)
import           Pos.Update.Network.Types (ProposalMsgTag (..), VoteMsgTag (..))
import           Pos.Util.Relay           (DataMsg, InvMsg, Relay (..), ReqMsg,
                                           handleDataL, handleInvL, handleReqL)
import           Pos.WorkMode             (WorkMode)

-- | Listeners for requests related to update system
usListeners
    :: (WorkMode ssc m)
    => [ListenerAction BiP m]
usListeners =
    [ handleInvProposal
    , handleReqProposal
    , handleDataProposal

    , handleInvVote
    , handleReqVote
    , handleDataVote
    ]

----------------------------------------------------------------------------
-- UpdateProposal listeners
----------------------------------------------------------------------------

handleInvProposal :: WorkMode ssc m => ListenerAction BiP m
handleInvProposal = ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg UpId ProposalMsgTag) ->
    handleInvL i peerId sendActions

handleReqProposal :: WorkMode ssc m => ListenerAction BiP m
handleReqProposal = ListenerActionOneMsg $ \peerId sendActions (i :: ReqMsg UpId ProposalMsgTag) ->
    handleReqL i peerId sendActions

handleDataProposal :: WorkMode ssc m => ListenerAction BiP m
handleDataProposal = ListenerActionOneMsg $ \peerId sendActions (i :: DataMsg UpId UpdateProposal) ->
    handleDataL i peerId sendActions

instance WorkMode ssc m =>
         Relay m ProposalMsgTag UpId UpdateProposal where
    contentsToTag _ = pure ProposalMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    -- TODO: maybe somehow check that versions are not decreasing or whatevs?
    verifyDataContents UpdateProposal{..} = pure VerSuccess

    handleInv _ = isProposalNeeded
    handleReq _ = getLocalProposal
    handleData proposal _ = isRight <$> processProposal proposal

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

handleInvVote :: WorkMode ssc m => ListenerAction BiP m
handleInvVote = ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg VoteId VoteMsgTag) ->
    handleInvL i peerId sendActions

handleReqVote :: WorkMode ssc m => ListenerAction BiP m
handleReqVote = ListenerActionOneMsg $ \peerId sendActions (i :: ReqMsg VoteId VoteMsgTag) ->
    handleReqL i peerId sendActions

handleDataVote :: WorkMode ssc m => ListenerAction BiP m
handleDataVote = ListenerActionOneMsg $ \peerId sendActions (i :: DataMsg VoteId UpdateVote) ->
    handleDataL i peerId sendActions

instance WorkMode ssc m =>
         Relay m VoteMsgTag VoteId UpdateVote where
    contentsToTag _ = pure VoteMsgTag

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents UpdateVote{..} = pure VerSuccess

    handleInv _ (id, pk, dec) = isVoteNeeded id pk dec
    handleReq _ (id, pk, dec) = getLocalVote id pk dec
    handleData uv _ = isRight <$> processVote uv
