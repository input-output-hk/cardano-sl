-- | Functions for operating with messages of update system

{-# LANGUAGE RankNTypes #-}

module Pos.Client.Update.Network
       ( submitVote
       , submitUpdateProposal
       ) where

import           Universum

import           Formatting (sformat, (%))
import           System.Wlog (logInfo)

import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..))
import           Pos.Communication.Relay (invReqDataFlowTK)
import           Pos.Crypto (SafeSigner, SignTag (SignUSVote), hash, hashHexF, safeSign,
                             safeToPublic)
import           Pos.DB.Class (MonadGState)
import           Pos.Update (UpId, UpdateProposal, UpdateVote (..), mkVoteId)
import           Pos.WorkMode.Class (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m
    -> UpdateVote
    -> m ()
submitVote enqueue voteUpd = do
    void $ invReqDataFlowTK
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId voteUpd)
        voteUpd

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m
    -> [SafeSigner]
    -> UpdateProposal
    -> m ()
submitUpdateProposal enqueue ss prop = do
    let upid = hash prop
    let votes = constructVotes upid
    sendUpdateProposal enqueue upid prop votes
  where
    createVote upid (signer, signature) =
        UpdateVote
            { uvKey        = safeToPublic signer
            , uvProposalId = upid
            , uvDecision   = True
            , uvSignature  = signature
            }
    constructVotes :: UpId -> [UpdateVote]
    constructVotes upid =
        let signatures = map (\s -> safeSign SignUSVote s (upid, True)) ss
        in map (createVote upid) (zip ss signatures)

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal enqueue upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%hashHexF) upid
    void $ invReqDataFlowTK
        "UpdateProposal"
        enqueue
        (MsgMPC OriginSender)
        upid
        (proposal, votes)
