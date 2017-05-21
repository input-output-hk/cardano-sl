{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Specs
       ( sendTxOuts
       , sendVoteOuts
       , sendProposalOuts
       , createOutSpecs
       ) where

import           Node.Message                  (Message (..))
import           Universum

import           Pos.Communication.Message     ()
import           Pos.Communication.Protocol    (OutSpecs, convH, toOutSpecs)
import           Pos.Communication.Types.Relay (InvOrData, InvOrDataTK, ReqMsg)
import           Pos.Txp.Core.Types            (TxId)
import           Pos.Txp.Network.Types         (TxMsgContents (..))
import           Pos.Update.Core.Types         (UpId, UpdateProposal, UpdateVote, VoteId)

createOutSpecs :: forall key contents .
               ( Message (InvOrData key contents)
               , Message (ReqMsg key))
               => Proxy (InvOrData key contents)
               -> OutSpecs
createOutSpecs proxy = toOutSpecs [convH proxy (toReqProxy proxy)]
  where
    toReqProxy :: Proxy (InvOrData key contents) -> Proxy (ReqMsg key)
    toReqProxy _ = Proxy

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK TxId TxMsgContents))

sendVoteOuts :: OutSpecs
sendVoteOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK VoteId UpdateVote))

sendProposalOuts :: OutSpecs
sendProposalOuts = createOutSpecs (Proxy :: Proxy (InvOrDataTK UpId (UpdateProposal, [UpdateVote])))
