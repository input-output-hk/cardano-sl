{-# LANGUAGE Rank2Types          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Communication.Specs
       ( sendTxOuts
       , sendVoteOuts
       , sendProposalOuts
       , allOutSpecs
       , createOutSpecs
       ) where

import           Node.Message                  (Message (..))
import           Universum

import           Pos.Communication.Message     ()
import           Pos.Communication.Protocol    (OutSpecs, convH, toOutSpecs)
import           Pos.Communication.Types.Relay (InvOrData, ReqMsg)
import           Pos.Ssc.GodTossing.Types      (GtMsgContents (..), GtTag (..))
import           Pos.Txp.Core.Types            (TxId)
import           Pos.Txp.Network.Types         (TxMsgContents (..), TxMsgTag (..))
import           Pos.Types                     (StakeholderId)
import           Pos.Update.Core.Types         (UpId, UpdateProposal, UpdateVote, VoteId)
import           Pos.Update.Network.Types      (ProposalMsgTag (..), VoteMsgTag (..))

createOutSpecs :: forall tag id contents .
               ( Message (InvOrData tag id contents)
               , Message (ReqMsg id tag))
               => Proxy (InvOrData tag id contents)
               -> OutSpecs
createOutSpecs proxy = toOutSpecs [convH proxy (toReqProxy proxy)]
  where
    toReqProxy :: Proxy (InvOrData tag id contents) -> Proxy (ReqMsg id tag)
    toReqProxy _ = Proxy

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs (Proxy :: Proxy (InvOrData TxMsgTag TxId TxMsgContents))

sendVoteOuts :: OutSpecs
sendVoteOuts = createOutSpecs (Proxy :: Proxy (InvOrData VoteMsgTag VoteId UpdateVote))

sendProposalOuts :: OutSpecs
sendProposalOuts = createOutSpecs (Proxy :: Proxy (InvOrData ProposalMsgTag UpId (UpdateProposal, [UpdateVote])))

allOutSpecs :: OutSpecs
allOutSpecs = mconcat [
      sendTxOuts
    , sendVoteOuts
    , sendProposalOuts
    , createOutSpecs (Proxy :: Proxy (InvOrData GtTag StakeholderId GtMsgContents))
    ]
