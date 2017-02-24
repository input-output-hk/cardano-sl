{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods

module Pos.Communication.Methods
       ( sendTx
       , sendVote
       , sendUpdateProposal
       ) where

import           Formatting                 (build, sformat, (%))
import           System.Wlog                (logInfo)
import           Universum

import           Pos.Binary.Communication   ()
import           Pos.Binary.Relay           ()
import           Pos.Binary.Types           ()
import           Pos.Communication.Message  ()
import           Pos.Communication.Protocol (SendActions)
import           Pos.Communication.Relay    (invReqDataFlow)
import           Pos.Crypto                 (encodeHash, hash)
import           Pos.DHT.Model              (DHTNode)
import           Pos.Txp.Core.Types         (TxAux)
import           Pos.Txp.Network.Types      (TxMsgContents (..), TxMsgTag (..))
import           Pos.Update                 (ProposalMsgTag (..), UpId, UpdateProposal,
                                             UpdateVote, VoteMsgTag (..), mkVoteId)
import           Pos.WorkMode               (MinWorkMode)


-- | Send Tx to given address.
sendTx :: (MinWorkMode m) => SendActions m -> DHTNode -> TxAux -> m ()
sendTx sendActions addr (tx,w,d) =
    invReqDataFlow "tx" sendActions addr TxMsgTag (hash tx) (TxMsgContents tx w d)

-- Send UpdateVote to given address.
sendVote :: (MinWorkMode m) => SendActions m -> DHTNode -> UpdateVote -> m ()
sendVote sendActions addr vote =
    invReqDataFlow "UpdateVote" sendActions addr VoteMsgTag (mkVoteId vote) vote

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: (MinWorkMode m)
    => SendActions m
    -> DHTNode
    -> UpId
    -> UpdateProposal
    -> [UpdateVote]
    -> m ()
sendUpdateProposal sendActions addr upid proposal votes = do
    logInfo $ sformat ("Announcing proposal with id "%build%
                        " (base64 is "%build%")")
        upid (encodeHash upid)
    invReqDataFlow "UpdateProposal" sendActions addr ProposalMsgTag upid (proposal, votes)
