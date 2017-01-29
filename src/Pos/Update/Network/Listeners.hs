{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usListeners
       , usStubListeners
       ) where

import           Data.Proxy               (Proxy (..))
import           Formatting               (build, sformat, (%))
import           Node                     (ListenerAction (..))
import           Serokell.Util.Verify     (VerificationRes (..))
import           System.Wlog              (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Relay         ()
import           Pos.Communication.BiP    (BiP)
import           Pos.Crypto               (hash)
import           Pos.Update.Core          (UpId, UpdateProposal (..), UpdateVote (..),
                                           VoteId)
import           Pos.Update.Logic.Local   (getLocalProposalNVotes, getLocalVote,
                                           isProposalNeeded, isVoteNeeded,
                                           processProposal, processVote)
import           Pos.Update.Network.Types (ProposalMsgTag (..), VoteMsgTag (..))
import           Pos.Util                 (stubListenerOneMsg)
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

usStubListeners
    :: (WithLogger m)
    => [ListenerAction BiP m]
usStubListeners =
    [ stubListenerOneMsg (Proxy :: Proxy (InvMsg UpId ProposalMsgTag))
    , stubListenerOneMsg (Proxy :: Proxy (ReqMsg UpId ProposalMsgTag))
    , stubListenerOneMsg
        (Proxy :: Proxy (DataMsg (UpdateProposal, [UpdateVote])))

    , stubListenerOneMsg (Proxy :: Proxy (InvMsg VoteId VoteMsgTag))
    , stubListenerOneMsg (Proxy :: Proxy (ReqMsg VoteId VoteMsgTag))
    , stubListenerOneMsg (Proxy :: Proxy (DataMsg UpdateVote))
    ]

----------------------------------------------------------------------------
-- UpdateProposal listeners
----------------------------------------------------------------------------

handleInvProposal :: WorkMode ssc m => ListenerAction BiP m
handleInvProposal =
    ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg UpId ProposalMsgTag) ->
    handleInvL i peerId sendActions

handleReqProposal :: WorkMode ssc m => ListenerAction BiP m
handleReqProposal = ListenerActionOneMsg $
    \peerId sendActions (i :: ReqMsg UpId ProposalMsgTag) ->
    handleReqL i peerId sendActions

handleDataProposal :: WorkMode ssc m => ListenerAction BiP m
handleDataProposal = ListenerActionOneMsg $
    \peerId sendActions (i :: DataMsg (UpdateProposal, [UpdateVote])) -> do
    logDebug $ sformat ("Received update proposal: "%build) i
    handleDataL i peerId sendActions

instance WorkMode ssc m =>
         Relay m ProposalMsgTag UpId (UpdateProposal, [UpdateVote]) where
    contentsToTag _ = pure ProposalMsgTag
    contentsToKey (up,_) = pure $ hash up

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents _ = pure VerSuccess

    handleInv _ = isProposalNeeded
    handleReq _ = getLocalProposalNVotes
    handleData (proposal, votes) = do
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

handleInvVote :: WorkMode ssc m => ListenerAction BiP m
handleInvVote = ListenerActionOneMsg $ \peerId sendActions (i :: InvMsg VoteId VoteMsgTag) ->
    handleInvL i peerId sendActions

handleReqVote :: WorkMode ssc m => ListenerAction BiP m
handleReqVote = ListenerActionOneMsg $ \peerId sendActions (i :: ReqMsg VoteId VoteMsgTag) ->
    handleReqL i peerId sendActions

handleDataVote :: WorkMode ssc m => ListenerAction BiP m
handleDataVote = ListenerActionOneMsg $ \peerId sendActions (i :: DataMsg UpdateVote) -> do
    logDebug $ sformat ("Received vote: "%build) i
    handleDataL i peerId sendActions

instance WorkMode ssc m =>
         Relay m VoteMsgTag VoteId UpdateVote where
    contentsToTag _ = pure VoteMsgTag
    contentsToKey UpdateVote{..} = pure (uvProposalId, uvKey, uvDecision)

    verifyInvTag _ = pure VerSuccess
    verifyReqTag _ = pure VerSuccess
    verifyDataContents UpdateVote{..} = pure VerSuccess

    handleInv _ (id, pk, dec) = isVoteNeeded id pk dec
    handleReq _ (id, pk, dec) = getLocalVote id pk dec
    handleData uv = do
        res <- processVote uv
        logProcess res
        pure $ isRight res
      where
        logProcess (Left cause) =
          logDebug $ sformat ("Processing of vote failed: "%build) cause
        logProcess (Right _) = logDebug $ "Processing of vote is successfull"
