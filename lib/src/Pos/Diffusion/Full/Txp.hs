{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Txp
       ( sendTx
       , txListeners
       , txOutSpecs
       ) where

import           Universum
import           Data.Tagged (Tagged)
import qualified Network.Broadcast.OutboundQueue as OQ

import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Txp ()
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlTxMsgContents)
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..), NodeId,
                                             MkListeners, OutSpecs)
import           Pos.Communication.Relay (invReqDataFlowTK, resOk,
                                          InvReqDataParams (..), invReqMsgType, Relay (..),
                                          relayListeners, MempoolParams (..),
                                          relayPropagateOut)
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Crypto (hash)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Txp.Network.Types (TxMsgContents (..))
import           Pos.Util.Trace (Trace, Severity)

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
