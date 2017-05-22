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
usRelays :: forall m. UpdateMode m
    => [Relay m]
usRelays = [proposalRelay, voteRelay]

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay
    :: UpdateMode m
    => Relay m
proposalRelay =
    InvReqData
        NoMempool $
        InvReqDataParams
           { contentsToKey = \(up, _) -> pure . tag  $ hash up
           , handleInv = isProposalNeeded . unTagged
           , handleReq = getLocalProposalNVotes . unTagged
           , handleData = \(proposal, votes) -> do
                 res <- processProposal proposal
                 logProp res
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

    logProp (Left cause) =
        logDebug $ sformat ("Processing of proposal failed: "%build) cause
    logProp (Right _) = logDebug "Processing of proposal is successfull"

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

voteRelay
    :: UpdateMode m
    => Relay m
voteRelay =
    InvReqData
        NoMempool $
        InvReqDataParams
           { contentsToKey = \UpdateVote{..} ->
                 pure $ tag (uvProposalId, uvKey, uvDecision)
           , handleInv = \(Tagged (id, pk, dec)) -> isVoteNeeded id pk dec
           , handleReq = \(Tagged (id, pk, dec)) -> getLocalVote id pk dec
           , handleData = \uv -> do
                 res <- processVote uv
                 logProcess res
                 pure $ isRight res
           }
  where
    tag = tagWith (Proxy :: Proxy UpdateVote)
    logProcess (Left cause) =
      logDebug $ sformat ("Processing of vote failed: "%build) cause
    logProcess (Right _) = logDebug $ "Processing of vote is successfull"
