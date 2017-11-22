{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal

       , updateListeners
       ) where

import           Universum
import           Formatting (sformat, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           System.Wlog (logInfo)

import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Txp ()
import           Pos.Core.Configuration (HasCoreConfiguration)
import           Pos.Core.Configuration.BlockVersionData (HasGenesisBlockVersionData)
import           Pos.Core.Configuration.GenesisData (HasGenesisData)
import           Pos.Core.Configuration.GenesisHash (HasGenesisHash)
import           Pos.Core.Configuration.GeneratedSecrets (HasGeneratedSecrets)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Communication.Limits (HasUpdateLimits)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..),
                                             NodeId, MkListeners)
import           Pos.Communication.Relay (invReqDataFlowTK, MinRelayWorkMode,
                                          Relay (..), relayListeners,
                                          InvReqDataParams (..), MempoolParams (..))
import           Pos.Crypto (hashHexF)
import           Pos.Crypto.Configuration (HasCryptoConfiguration)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Network.Types (Bucket)
import           Pos.Update (UpId, UpdateProposal, UpdateVote, mkVoteId)

-- Send UpdateVote to given addresses.
sendVote
    :: ( MinRelayWorkMode m
       , HasUpdateLimits m
       , HasCoreConfiguration
       , HasGenesisData
       , HasGenesisHash
       , HasGeneratedSecrets
       , HasGenesisBlockVersionData
       , HasProtocolConstants
       , HasCryptoConfiguration
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
       , HasUpdateLimits m
       , HasCoreConfiguration
       , HasGenesisData
       , HasGenesisHash
       , HasGeneratedSecrets
       , HasGenesisBlockVersionData
       , HasProtocolConstants
       , HasCryptoConfiguration
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
       , HasUpdateLimits m
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
       , HasUpdateLimits m
       )
    => Logic m
    -> [Relay m]
usRelays logic = [proposalRelay logic, voteRelay logic]

----------------------------------------------------------------------------
-- UpdateProposal relays
----------------------------------------------------------------------------

proposalRelay
    :: ( DiffusionWorkMode m
       , HasUpdateLimits m
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
           }
  where
    kv = postVote logic
