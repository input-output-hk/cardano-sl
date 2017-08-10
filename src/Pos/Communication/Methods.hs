-- | Wrappers on top of communication methods

{-# LANGUAGE RankNTypes #-}

module Pos.Communication.Methods
       ( sendTx
       , sendVote
       , sendUpdateProposal
       ) where

import           Formatting                 (sformat, (%))
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Binary.Core            ()
import           Pos.Binary.Relay           ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..))
import           Pos.Communication.Relay    (invReqDataFlowTK)
import           Pos.Crypto                 (hash, hashHexF)
import           Pos.DB.Class               (MonadGState)
import           Pos.Txp.Core.Types         (TxAux (..))
import           Pos.Txp.Network.Types      (TxMsgContents (..))
import           Pos.Update                 (UpId, UpdateProposal, UpdateVote, mkVoteId)
import           Pos.WorkMode.Class         (MinWorkMode)


-- | Send Tx to given addresses.
sendTx
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m -> TxAux -> m ()
sendTx enqueue txAux =
    invReqDataFlowTK
        "tx"
        enqueue
        (MsgTransaction OriginSender)
        (hash $ taTx txAux)
        (TxMsgContents txAux)

-- Send UpdateVote to given addresses.
sendVote
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m -> UpdateVote -> m ()
sendVote enqueue vote =
    invReqDataFlowTK
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId vote)
        vote

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
    invReqDataFlowTK
        "UpdateProposal"
        enqueue
        (MsgMPC OriginSender)
        upid
        (proposal, votes)
