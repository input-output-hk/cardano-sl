-- | Functions for operating with messages of update system

module Pos.Wallet.Update
       ( submitVote
       , submitUpdateProposal
       , sendVoteOuts
       , sendProposalOuts
       ) where

import           Mockable                   (forConcurrently)
import           Universum

import           Pos.Binary                 ()

import           Pos.Communication.Methods  (sendUpdateProposal, sendVote)
import           Pos.Communication.Protocol (NodeId, SendActions)
import           Pos.Communication.Specs    (sendProposalOuts, sendVoteOuts)
import           Pos.DB.Limits              (MonadDBLimits)

import           Pos.Crypto                 (SafeSigner, SignTag (SignUSVote), hash,
                                             safeSign, safeToPublic)
import           Pos.Update                 (UpdateProposal, UpdateVote (..))
import           Pos.WorkMode.Class         (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: (MinWorkMode m, MonadDBLimits m)
    => SendActions m
    -> [NodeId]
    -> UpdateVote
    -> m ()
submitVote sendActions na voteUpd = do
    void $ forConcurrently na $
        \addr -> sendVote sendActions addr voteUpd

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: (MinWorkMode m, MonadDBLimits m)
    => SendActions m
    -> SafeSigner
    -> [NodeId]
    -> UpdateProposal
    -> m ()
submitUpdateProposal sendActions ss na prop = do
    let upid = hash prop
    let initUpdVote = UpdateVote
            { uvKey        = safeToPublic ss
            , uvProposalId = upid
            , uvDecision   = True
            , uvSignature  = safeSign SignUSVote ss (upid, True)
            }
    void $ forConcurrently na $
        \addr -> sendUpdateProposal sendActions addr upid prop [initUpdVote]
