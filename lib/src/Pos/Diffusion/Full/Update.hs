{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal
       ) where

import           Formatting (sformat, (%))
import           System.Wlog (logInfo)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Relay ()
import           Pos.Binary.Txp ()
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..))
import           Pos.Communication.Relay (invReqDataFlowTK)
import           Pos.Crypto (hashHexF)
import           Pos.Update (UpId, UpdateProposal, UpdateVote, mkVoteId)
import           Pos.WorkMode.Class (WorkMode)

-- Send UpdateVote to given addresses.
sendVote
    :: ( WorkMode ctx m )
    => EnqueueMsg m
    -> UpdateVote
    -> m ()
sendVote enqueue vote =
    void $ invReqDataFlowTK
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId vote)
        vote

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: ( WorkMode ctx m )
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
