-- | Functions for operating with messages of update system

module Pos.Communication.Update
       ( submitVote
       , submitUpdateProposal
       ) where

import           Universum

import           Mockable                   (forConcurrently)

import           Pos.Binary                 ()
import           Pos.Communication.Methods  (sendUpdateProposal, sendVote)
import           Pos.Communication.Protocol (NodeId, SendActions)
import           Pos.Crypto                 (SafeSigner, SignTag (SignUSVote), hash,
                                             safeSign, safeToPublic)
import           Pos.DB.Class               (MonadGStateCore)
import           Pos.Update                 (UpdateProposal, UpdateVote (..))
import           Pos.WorkMode.Class         (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: (MinWorkMode m, MonadGStateCore m)
    => SendActions m
    -> [NodeId]
    -> UpdateVote
    -> m ()
submitVote sendActions na voteUpd = do
    void $ forConcurrently na $
        \addr -> sendVote sendActions addr voteUpd

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: (MinWorkMode m, MonadGStateCore m)
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
