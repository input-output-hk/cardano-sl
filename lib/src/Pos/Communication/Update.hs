-- | Functions for operating with messages of update system

{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Update
       ( submitVote
       , submitUpdateProposal
       ) where

import           Universum

import           Pos.Binary                 ()
import           Pos.Communication.Methods  (sendUpdateProposal, sendVote)
import           Pos.Communication.Protocol (EnqueueMsg)
import           Pos.Crypto                 (SafeSigner, SignTag (SignUSVote), hash,
                                             safeSign, safeToPublic)
import           Pos.DB.Class               (MonadGState)
import           Pos.Update                 (UpId, UpdateProposal, UpdateVote (..))
import           Pos.WorkMode.Class         (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m
    -> UpdateVote
    -> m ()
submitVote enqueue voteUpd = do
    sendVote enqueue voteUpd

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
