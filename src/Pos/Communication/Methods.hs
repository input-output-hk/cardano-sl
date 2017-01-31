{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Wrappers on top of communication methods

module Pos.Communication.Methods
       ( sendTx
       , sendVote
       , sendUpdateProposal
       , sendTxOuts
       , sendVoteOuts
       , sendProposalOuts
       ) where

import           Data.Proxy                  (Proxy (..))
import           Formatting                  (build, sformat, shown, stext, (%))
import           Mockable                    (handleAll)
import           Node.Message                (Message (..))
import           System.Wlog                 (logInfo, logWarning)
import           Universum

import           Pos.Binary.Class            (Bi)
import           Pos.Binary.Communication    ()
import           Pos.Binary.Relay            ()
import           Pos.Binary.Types            ()
import           Pos.Communication.Message   ()
import           Pos.Communication.Protocol  (ConversationActions (..), OutSpecs,
                                              SendActions, convH, toOutSpecs)
import           Pos.Communication.Relay     (DataMsg (..), InvMsg (..), InvOrData,
                                              ReqMsg (..))
import           Pos.Crypto                  (encodeHash, hash)
import           Pos.DHT.Model               (DHTNode, converseToNode)
import           Pos.Txp.Types.Communication (TxMsgContents (..), TxMsgTag (..))
import           Pos.Types                   (TxAux, TxId)
import           Pos.Update                  (ProposalMsgTag (..), UpId, UpdateProposal,
                                              UpdateVote, VoteId, VoteMsgTag (..),
                                              mkVoteId)
import           Pos.WorkMode                (MinWorkMode)

createOutSpecs :: forall tag id contents .
                  ( Message (InvOrData tag id contents)
                  , Message (ReqMsg id tag))
                => OutSpecs
createOutSpecs =
    toOutSpecs [ convH (Proxy :: Proxy (InvOrData tag id contents))
                       (Proxy :: Proxy (ReqMsg id tag))
               ]

invReqDataFlow :: ( Message (InvOrData tag id contents)
                  , Message (ReqMsg id tag)
                  , Buildable id
                  , MinWorkMode m
                  , Bi tag, Bi id
                  , Bi (InvOrData tag id contents)
                  , Bi (ReqMsg id tag))
               => Text -> SendActions m -> DHTNode -> tag -> id -> contents -> m ()
invReqDataFlow what sendActions addr tag id dt = handleAll handleE $
    converseToNode sendActions addr convHandler
  where
    convHandler _
      ca@(ConversationActions{..}::(ConversationActions (InvOrData tag id contents) (ReqMsg id tag)) m) = do
        send $ Left $ InvMsg tag (one id)
        recv >>= maybe (handleE ("node didn't reply by ReqMsg"::Text)) (replyByData ca)
    replyByData ca (ReqMsg _ key) = do
      send ca $ Right $ DataMsg dt id
    handleE e =
      logWarning $
        sformat ("Error sending"%stext%", id = "%build%" to "%shown%": "%shown) what id addr e

sendTxOuts :: OutSpecs
sendTxOuts = createOutSpecs @TxMsgTag @TxId @TxMsgContents

-- | Send Tx to given address.
sendTx :: (MinWorkMode m) => SendActions m -> DHTNode -> TxAux -> m ()
sendTx sendActions addr (tx,w,d) =
    invReqDataFlow "tx" sendActions addr TxMsgTag (hash tx) (TxMsgContents tx w d)

sendVoteOuts :: OutSpecs
sendVoteOuts = createOutSpecs @VoteMsgTag @VoteId @UpdateVote

-- Send UpdateVote to given address.
sendVote :: (MinWorkMode m) => SendActions m -> DHTNode -> UpdateVote -> m ()
sendVote sendActions addr vote =
    invReqDataFlow "UpdateVote" sendActions addr VoteMsgTag (mkVoteId vote) vote

sendProposalOuts :: OutSpecs
sendProposalOuts = createOutSpecs @ProposalMsgTag @UpId @(UpdateProposal, [UpdateVote])

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
