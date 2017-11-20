{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}

module Pos.Diffusion.Full.Update
       ( sendVote
       , sendUpdateProposal
       ) where

import           Formatting (sformat, (%))
import           System.Wlog (logInfo)
import           Universum

import           Pos.Binary.Communication ()
import           Pos.Binary.Core ()
import           Pos.Binary.Relay ()
import           Pos.Binary.Txp ()
import           Pos.Core.Configuration (HasCoreConfiguration)
import           Pos.Core.Configuration.BlockVersionData (HasGenesisBlockVersionData)
import           Pos.Core.Configuration.GenesisData (HasGenesisData)
import           Pos.Core.Configuration.GenesisHash (HasGenesisHash)
import           Pos.Core.Configuration.GeneratedSecrets (HasGeneratedSecrets)
import           Pos.Core.Configuration.Protocol (HasProtocolConstants)
import           Pos.Communication.Limits (HasUpdateLimits)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (EnqueueMsg, MsgType (..), Origin (..))
import           Pos.Communication.Relay (invReqDataFlowTK, MinRelayWorkMode)
import           Pos.Crypto (hashHexF)
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
