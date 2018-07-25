{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Txp
       ( sendTx
       , txListeners
       , txOutSpecs
       ) where

import           Data.Tagged (Tagged)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Communication.Limits (mlTxMsgContents)
import           Pos.Core.Txp (TxAux (..), TxId, TxMsgContents (..))
import           Pos.Crypto (hash)
import           Pos.Infra.Communication.Protocol (EnqueueMsg, MkListeners,
                     MsgType (..), NodeId, Origin (..), OutSpecs)
import           Pos.Infra.Communication.Relay (InvReqDataParams (..),
                     MempoolParams (..), Relay (..), invReqDataFlowTK,
                     invReqMsgType, relayListeners, relayPropagateOut, resOk)
import           Pos.Infra.Network.Types (Bucket)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Util.Trace (Severity, Trace)

-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx :: Trace IO (Severity, Text) -> EnqueueMsg -> TxAux -> IO Bool
sendTx logTrace enqueue txAux = do
    anySucceeded <$> invReqDataFlowTK
        logTrace
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

txListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
txListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (txRelays logic)

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
txOutSpecs :: Logic IO -> OutSpecs
txOutSpecs logic = relayPropagateOut (txRelays logic)

txInvReqDataParams
    :: Logic IO
    -> InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents
txInvReqDataParams logic =
    InvReqDataParams
       { invReqMsgType = MsgTransaction
       , contentsToKey = KV.toKey (postTx logic)
       , handleInv = \_ -> KV.handleInv (postTx logic)
       , handleReq = \_ -> KV.handleReq (postTx logic)
       , handleData = \_ -> KV.handleData (postTx logic)
       , irdpMkLimit = mlTxMsgContents <$> getAdoptedBVData logic
       }

txRelays :: Logic IO -> [Relay]
txRelays logic = pure $
    -- Previous implementation had KeyMempool, but mempool messages are never
    -- used so we drop it.
    InvReqData NoMempool (txInvReqDataParams logic)
