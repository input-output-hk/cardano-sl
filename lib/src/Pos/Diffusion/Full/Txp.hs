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
import           Pos.Communication.Relay (invReqDataFlowTK, resOk, MinRelayWorkMode,
                                          InvReqDataParams (..), invReqMsgType, Relay (..),
                                          relayListeners, MempoolParams (..),
                                          relayPropagateOut)
import           Pos.Core.Txp (TxAux (..), TxId)
import           Pos.Crypto (hash)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Txp.Network.Types (TxMsgContents (..))

-- | Send Tx to given addresses.
-- Returns 'True' if any peer accepted and applied this transaction.
sendTx
    :: ( MinRelayWorkMode m
       )
    => EnqueueMsg m
    -> TxAux
    -> m Bool
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

txListeners
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
txListeners logic oq enqueue = relayListeners oq enqueue (txRelays logic)

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
txOutSpecs
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OutSpecs
txOutSpecs logic = relayPropagateOut (txRelays logic)

txInvReqDataParams
    :: DiffusionWorkMode m
    => Logic m
    -> InvReqDataParams (Tagged TxMsgContents TxId) TxMsgContents m
txInvReqDataParams logic =
    InvReqDataParams
       { invReqMsgType = MsgTransaction
       , contentsToKey = KV.toKey (postTx logic)
       , handleInv = \_ -> KV.handleInv (postTx logic)
       , handleReq = \_ -> KV.handleReq (postTx logic)
       , handleData = \_ -> KV.handleData (postTx logic)
       , irdpMkLimit = mlTxMsgContents <$> getAdoptedBVData logic
       }

txRelays
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> [Relay m]
txRelays logic = pure $
    -- Previous implementation had KeyMempool, but mempool messages are never
    -- used so we drop it.
    InvReqData NoMempool (txInvReqDataParams logic)
