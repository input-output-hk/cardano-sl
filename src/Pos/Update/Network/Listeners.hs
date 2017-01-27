{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usListeners
       , usStubListeners
       ) where

import           Data.Proxy                       (Proxy (..))
import           Formatting                       (build, sformat, (%))
import           Node                             (ListenerAction (..))
import           Serokell.Util.Verify             (VerificationRes (..))
import           System.Wlog                      (WithLogger, logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication         ()
import           Pos.Binary.Relay                 ()
import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Message        ()
import           Pos.Communication.Relay          (DataMsg, InvMsg, Relay (..), ReqMsg,
                                                   handleDataL, handleInvL, handleReqL)
import           Pos.Communication.Types.Protocol (PeerId)
import           Pos.Update.Core                  (UpId, UpdateProposal (..),
                                                   UpdateVote (..), VoteId)
import           Pos.Update.Logic.Local           (getLocalProposalNVotes, getLocalVote,
                                                   isProposalNeeded, isVoteNeeded,
                                                   processProposal, processVote)
import           Pos.Update.Network.Types         (ProposalMsgTag (..), VoteMsgTag (..))
import           Pos.Util                         (stubListenerOneMsg)
import           Pos.WorkMode                     (WorkMode)

-- | Listeners for requests related to update system
usListeners
    :: (WorkMode ssc m)
    => [ListenerAction BiP PeerId m]
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
    => [ListenerAction BiP PeerId m]
usStubListeners =
    [ stubListenerOneMsg (Proxy :: Proxy (InvMsg UpId ProposalMsgTag))
    , stubListenerOneMsg (Proxy :: Proxy (ReqMsg UpId ProposalMsgTag))
    , stubListenerOneMsg
        (Proxy :: Proxy (DataMsg UpId (UpdateProposal, [UpdateVote])))

    , stubListenerOneMsg (Proxy :: Proxy (InvMsg VoteId VoteMsgTag))
    , stubListenerOneMsg (Proxy :: Proxy (ReqMsg VoteId VoteMsgTag))
    , stubListenerOneMsg (Proxy :: Proxy (DataMsg VoteId UpdateVote))
    ]

----------------------------------------------------------------------------
-- UpdateProposal listeners
----------------------------------------------------------------------------

handleInvProposal :: WorkMode ssc m => ListenerAction BiP PeerId m
handleInvProposal = ListenerActionOneMsg $ \_ peerId sendActions (i :: InvMsg UpId ProposalMsgTag) ->
    handleInvL i peerId sendActions

handleReqProposal :: WorkMode ssc m => ListenerAction BiP PeerId m
handleReqProposal = ListenerActionOneMsg $
    \_ peerId sendActions (i :: ReqMsg UpId ProposalMsgTag) ->
    handleReqL i peerId sendActions

handleDataProposal :: WorkMode ssc m => ListenerAction BiP PeerId m
handleDataProposal = ListenerActionOneMsg $
    \_ peerId sendActions (i :: DataMsg UpId (UpdateProposal, [UpdateVote])) -> do
    logDebug $ sformat ("Received update proposal: "%build) i
    handleDataL i peerId sendActions

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

handleInvVote :: WorkMode ssc m => ListenerAction BiP PeerId m
handleInvVote = ListenerActionOneMsg $ \_ peerId sendActions (i :: InvMsg VoteId VoteMsgTag) ->
    handleInvL i peerId sendActions

handleReqVote :: WorkMode ssc m => ListenerAction BiP PeerId m
handleReqVote = ListenerActionOneMsg $ \_ peerId sendActions (i :: ReqMsg VoteId VoteMsgTag) ->
    handleReqL i peerId sendActions

handleDataVote :: WorkMode ssc m => ListenerAction BiP PeerId m
handleDataVote = ListenerActionOneMsg $ \_ peerId sendActions (i :: DataMsg VoteId UpdateVote) -> do
    logDebug $ sformat ("Received vote: "%build) i
    handleDataL i peerId sendActions

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
