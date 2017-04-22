
-- | Wrappers on top of communication methods

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
import           Pos.Communication.Protocol (SendActions, NodeId)
import           Pos.Communication.Relay    (invReqDataFlow)
import           Pos.Crypto                 (hash, hashHexF)
import           Pos.DB.Limits              (MonadDBLimits)
import           Pos.Txp.Core.Types         (TxAux)
import           Pos.Txp.Network.Types      (TxMsgContents (..), TxMsgTag (..))
import           Pos.Update                 (ProposalMsgTag (..), UpId, UpdateProposal,
                                             UpdateVote, VoteMsgTag (..), mkVoteId)
import           Pos.WorkMode               (MinWorkMode)


-- | Send Tx to given address.
sendTx
    :: (MinWorkMode m, MonadDBLimits m)
    => SendActions m -> NodeId -> TxAux -> m ()
sendTx sendActions addr (tx,w,d) =
    invReqDataFlow "tx" sendActions addr TxMsgTag (hash tx) (TxMsgContents tx w d)

-- Send UpdateVote to given address.
sendVote
    :: (MinWorkMode m, MonadDBLimits m)
    => SendActions m -> NodeId -> UpdateVote -> m ()
sendVote sendActions addr vote =
    invReqDataFlow "UpdateVote" sendActions addr VoteMsgTag (mkVoteId vote) vote

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m, MonadDBLimits m)
    => SendActions m
    -> NodeId
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal sendActions addr upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%hashHexF) upid
    invReqDataFlow
        "UpdateProposal"
        sendActions
        addr
        ProposalMsgTag
        upid
        (proposal, votes)
