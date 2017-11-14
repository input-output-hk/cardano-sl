-- | Wrappers on top of communication methods

{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Communication.Methods
       ( sendTx
       , sendVote
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
import           Pos.Communication.Relay (invReqDataFlowTK, resOk)
import           Pos.Core.Txp (TxAux (..))
import           Pos.Crypto (hash, hashHexF)
import           Pos.DB.Class (MonadGState)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import           Pos.Update (UpId, UpdateProposal, UpdateVote, mkVoteId)
import           Pos.WorkMode.Class (MinWorkMode)


-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m -> TxAux -> m Bool
sendTx enqueue txAux = do
    anySucceeded <$> invReqDataFlowTK
        "tx"
        enqueue
        (MsgTransaction OriginSender)
        (hash $ taTx txAux)
        (TxMsgContents txAux)
  where
    anySucceeded outcome =
        not $ null
        [ ()
        | Right (Just peerResponse) <- toList outcome
        , resOk peerResponse
        ]

-- Send UpdateVote to given addresses.
sendVote
    :: (MinWorkMode m, MonadGState m)
    => EnqueueMsg m -> UpdateVote -> m ()
sendVote enqueue vote =
    void $ invReqDataFlowTK
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
    void $ invReqDataFlowTK
        "UpdateProposal"
        enqueue
        (MsgMPC OriginSender)
        upid
        (proposal, votes)
