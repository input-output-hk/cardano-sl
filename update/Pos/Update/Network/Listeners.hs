{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usRelays
       ) where

import           Universum

import           Data.Tagged (Tagged (..), tagWith)
import           Formatting (build, sformat, (%))
import           System.Wlog (WithLogger, logNotice, logWarning)

import           Pos.Communication.Relay (InvReqDataParams (..), MempoolParams (..), Relay (..))
import           Pos.Communication.Types.Protocol (MsgType (..))
import           Pos.Core.Update (UpdateProposal (..), UpdateVote (..))
import           Pos.Crypto (hash)
import           Pos.Update.Logic.Local (getLocalProposalNVotes, getLocalVote, isProposalNeeded,
                                         isVoteNeeded, processProposal, processVote)
import           Pos.Update.Mode (UpdateMode)

-- | Relays for data related to update system
usRelays :: forall ctx m. UpdateMode ctx m
    => [Relay m]
usRelays = [proposalRelay, voteRelay]

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay
    :: forall ctx m. UpdateMode ctx m
    => Relay m
proposalRelay =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = \(up, _) -> pure . tag  $ hash up
           , handleInv = \_ -> isProposalNeeded . unTagged
           , handleReq = \_ -> getLocalProposalNVotes . unTagged
           , handleData = \_ (proposal, votes) -> do
                 res <- processProposal proposal
                 logProp proposal res
                 let processed = isRight res
                 processed <$ when processed (mapM_ processVoteLog votes)
           }
  where
    tag = tagWith (Proxy :: Proxy (UpdateProposal, [UpdateVote]))

    processVoteLog :: UpdateVote -> m ()
    processVoteLog vote = processVote vote >>= logVote vote
    logVote vote (Left cause) =
        logWarning $ sformat ("Proposal is accepted but vote "%build%
                              " is rejected, the reason is: "%build)
                     vote cause
    logVote vote (Right _) = logVoteAccepted vote

    logProp prop (Left cause) =
        logWarning $ sformat ("Processing of proposal "%build%
                              " failed, the reason is: "%build)
              prop cause
    -- Update proposals are accepted rarely (at least before Shelley),
    -- so it deserves 'Notice' severity.
    logProp prop (Right _) =
        logNotice $ sformat ("Processing of proposal "%build%" is successful")
              prop

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

voteRelay ::
       forall ctx m. UpdateMode ctx m
    => Relay m
voteRelay =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = \vote ->
                 pure $ tag (uvProposalId vote, uvKey vote, uvDecision vote)
           , handleInv = \_ (Tagged (id, pk, dec)) -> isVoteNeeded id pk dec
           , handleReq = \_ (Tagged (id, pk, dec)) -> getLocalVote id pk dec
           , handleData = \_ uv -> do
                 res <- processVote uv
                 logProcess uv res
                 pure $ isRight res
           }
  where
    tag = tagWith (Proxy :: Proxy UpdateVote)
    logProcess vote (Left cause) =
        logWarning $ sformat ("Processing of vote "%build%
                              "failed, the reason is: "%build)
                     vote cause
    logProcess vote (Right _) = logVoteAccepted vote

----------------------------------------------------------------------------
-- Helpers
----------------------------------------------------------------------------

-- Update votes are accepted rarely (at least before Shelley), so
-- it deserves 'Notice' severity.
logVoteAccepted :: WithLogger m => UpdateVote -> m ()
logVoteAccepted =
    logNotice . sformat ("Processing of vote "%build%"is successfull")
