-- | Functions for operating with messages of update system

module Pos.Wallet.Update
       ( submitVote
       , submitUpdateProposal
       , sendVoteOuts
       , sendProposalOuts
       ) where

import           Mockable                   (forConcurrently)
import           Pos.Util.TimeWarp          (NetworkAddress)
import           Universum

import           Pos.Binary                 ()

import           Pos.Communication.Methods  (sendProposalOuts, sendUpdateProposal,
                                             sendVote, sendVoteOuts)
import           Pos.Communication.Protocol (SendActions)
import           Pos.DHT.Model              (DHTNode)

import           Pos.Crypto                 (SecretKey, hash, sign, toPublic)
import           Pos.Update                 (UpdateProposal, UpdateVote (..))
import           Pos.WorkMode               (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: MinWorkMode m
    => SendActions m
    -> [DHTNode]
    -> UpdateVote
    -> m ()
submitVote sendActions na voteUpd = do
    void $ forConcurrently na $
        \addr -> sendVote sendActions addr voteUpd

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: MinWorkMode m
    => SendActions m
    -> SecretKey
    -> [DHTNode]
    -> UpdateProposal
    -> m ()
submitUpdateProposal sendActions sk na prop = do
    let upid = hash prop
    let initUpdVote = UpdateVote
            { uvKey        = toPublic sk
            , uvProposalId = upid
            , uvDecision   = True
            , uvSignature  = sign sk (upid, True)
            }
    void $ forConcurrently na $
        \addr -> sendUpdateProposal sendActions addr upid prop [initUpdVote]
