{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal

       , updateListeners
       , updateOutSpecs
       ) where

import           Universum
import           Formatting (sformat, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           System.Wlog (logInfo)

import           Pos.Core.Update (UpId, UpdateVote, UpdateProposal, mkVoteId)
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (mlUpdateVote, mlUpdateProposalAndVotes)
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..),
                                             NodeId, MkListeners, OutSpecs)
import           Pos.Communication.Relay (invReqDataFlowTK, MinRelayWorkMode,
                                          Relay (..), relayListeners,
                                          InvReqDataParams (..), MempoolParams (..),
                                          relayPropagateOut)
import           Pos.Crypto (hashHexF)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Update ()

-- Send UpdateVote to given addresses.
sendVote
    :: ( MinRelayWorkMode m
       )
    => EnqueueMsg m
    -> UpdateVote
    -> m ()
sendVote enqueue vote =
    void $ invReqDataFlowTK
        "UpdateVote"
        enqueue
        (MsgMPC OriginSender)
        (mkVoteId vote)
        vote

-- Send UpdateProposal to given address.
sendUpdateProposal
    :: ( MinRelayWorkMode m
       )
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

updateListeners
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
updateListeners logic oq enqueue = relayListeners oq enqueue (usRelays logic)

-- | Relays for data related to update system
usRelays
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> [Relay m]
usRelays logic = [proposalRelay logic, voteRelay logic]

-- | 'OutSpecs' for the update system, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
updateOutSpecs
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OutSpecs
updateOutSpecs logic = relayPropagateOut (usRelays logic)

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay
    :: ( DiffusionWorkMode m
       )
    => Logic m
    -> Relay m
proposalRelay logic =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = KV.toKey kv
           , handleInv = \_ -> KV.handleInv kv
           , handleReq = \_ -> KV.handleReq kv
           , handleData = \_ -> KV.handleData kv
           , irdpMkLimit = mlUpdateProposalAndVotes <$> getAdoptedBVData logic
           }
  where
    kv = postUpdate logic

----------------------------------------------------------------------------
-- UpdateVote listeners
----------------------------------------------------------------------------

voteRelay
    :: ( DiffusionWorkMode m )
    => Logic m
    -> Relay m
voteRelay logic =
    InvReqData
        NoMempool $
        InvReqDataParams
           { invReqMsgType = MsgTransaction
           , contentsToKey = KV.toKey kv
           , handleInv = \_ -> KV.handleInv kv
           , handleReq = \_ -> KV.handleReq kv
           , handleData = \_ -> KV.handleData kv
           , irdpMkLimit = pure mlUpdateVote
           }
  where
    kv = postVote logic
