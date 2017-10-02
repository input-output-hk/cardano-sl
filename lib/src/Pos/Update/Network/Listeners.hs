{-# LANGUAGE RankNTypes #-}
-- | Server which handles update system.

module Pos.Update.Network.Listeners
       ( usRelays
       ) where

import           Data.Tagged               (Tagged (..), tagWith)
import           Formatting                (build, sformat, (%))
import           System.Wlog               (logDebug, logWarning)
import           Universum

import           Pos.Binary.Communication  ()
import           Pos.Binary.Relay          ()
import           Pos.Communication.Types   (MsgType (..))
import           Pos.Communication.Limits  ()
import           Pos.Communication.Message ()
import           Pos.Communication.Relay   (InvReqDataParams (..), MempoolParams (..),
                                            Relay (..))
import           Pos.Crypto                (hash)
import           Pos.Update.Core           (UpdateProposal (..), UpdateVote (..))
import           Pos.Update.Logic.Local    (getLocalProposalNVotes, getLocalVote,
                                            isProposalNeeded, isVoteNeeded,
                                            processProposal, processVote)
import           Pos.Update.Mode           (UpdateMode)

-- | Relays for data related to update system
usRelays :: forall ctx m. UpdateMode ctx m
    => [Relay m]
usRelays = [proposalRelay, voteRelay]

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay
    :: UpdateMode ctx m
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
    processVoteLog = processVote >=> logVote
    logVote e@(Left cause) =
        e <$ logWarning (sformat ("Proposal accepted but vote "%build%" rejected") cause)
    logVote e@(Right _) =
        e <$ logDebug "Processing of proposal's vote is successfull"

    logProp prop (Left cause) =
        logDebug $ sformat ("Processing of proposal "%build%" with id "%build%" failed: "%build)
              prop (hash prop) cause
    logProp prop (Right _) =
        logDebug $ sformat ("Processing of proposal "%build%" with id "%build%" is successful")
              prop (hash prop)

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

voteRelay
    :: UpdateMode ctx m
    => Relay m
voteRelay =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = \UpdateVote{..} ->
                 pure $ tag (uvProposalId, uvKey, uvDecision)
           , handleInv = \_ (Tagged (id, pk, dec)) -> isVoteNeeded id pk dec
           , handleReq = \_ (Tagged (id, pk, dec)) -> getLocalVote id pk dec
           , handleData = \_ uv -> do
                 res <- processVote uv
                 logProcess res
                 pure $ isRight res
           }
  where
    tag = tagWith (Proxy :: Proxy UpdateVote)
    logProcess (Left cause) =
      logDebug $ sformat ("Processing of vote failed: "%build) cause
    logProcess (Right _) = logDebug $ "Processing of vote is successfull"
