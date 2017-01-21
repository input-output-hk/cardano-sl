-- | Functions for operating with transactions

module Pos.Wallet.Update
       ( submitVote
       , submitUpdateProposal
       ) where

import           Mockable                  (forConcurrently)
import           Node                      (SendActions)
import           Pos.Util.TimeWarp         (NetworkAddress)
import           Universum

import           Pos.Binary                ()
import           Pos.Communication.BiP     (BiP)
import           Pos.Communication.Methods (sendUpdateProposal, sendVote)
import           Pos.Crypto                (SecretKey, hash, sign, toPublic)
import           Pos.Update                (UpId, UpdateProposal, UpdateVote (..))
import           Pos.WorkMode              (MinWorkMode)

-- | Send UpdateVote to given addresses
submitVote
    :: MinWorkMode m
    => SendActions BiP m
    -> [NetworkAddress]
    -> UpdateVote
    -> m ()
submitVote sendActions na voteUpd = do
    void $ forConcurrently na $
        \addr -> sendVote sendActions addr voteUpd

-- | Send UpdateProposal with one positive vote to given addresses
submitUpdateProposal
    :: MinWorkMode m
    => SendActions BiP m
    -> SecretKey
    -> [NetworkAddress]
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
