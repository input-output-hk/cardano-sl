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
import           Pos.Update                 (UpdateProposal, UpdateVote (..))
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
    -> SafeSigner
    -> UpdateProposal
    -> m ()
submitUpdateProposal enqueue ss prop = do
    let upid = hash prop
    let initUpdVote = UpdateVote
            { uvKey        = safeToPublic ss
            , uvProposalId = upid
            , uvDecision   = True
            , uvSignature  = safeSign SignUSVote ss (upid, True)
            }
    sendUpdateProposal enqueue upid prop [initUpdVote]
