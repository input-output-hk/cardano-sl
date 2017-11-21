{-# LANGUAGE RankNTypes #-}

module Pos.Diffusion.Full.Ssc
    ( sscListeners
      -- TODO move the conversation starters here too (they're defined inline
      -- in Pos.Diffusion.Full).
    ) where

import           Universum

import           Data.Tagged (Tagged (..))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Node.Message.Class (Message)

import           Pos.Binary.Class (Bi)
import           Pos.Binary.Crypto ()
import           Pos.Binary.Infra ()
import           Pos.Binary.Ssc ()
import           Pos.Communication.Limits.Types (MessageLimited)
import           Pos.Communication.Relay (DataMsg, InvOrData, InvReqDataParams (..),
                                          MempoolParams (NoMempool), Relay (..), ReqMsg, ReqOrRes,
                                          relayListeners)
import           Pos.Communication.Types.Protocol (MsgType (..), NodeId, EnqueueMsg,
                                                   MkListeners)
import           Pos.Core (StakeholderId)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Network.Types (Bucket)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..), SscMessageConstraints)

sscListeners
    :: ( DiffusionWorkMode m )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
sscListeners logic oq enqueue = relayListeners oq enqueue (sscRelays logic)

sscRelays
    :: ( DiffusionWorkMode m
       , SscMessageConstraints m
       , MessageLimited (DataMsg MCVssCertificate) m
       , MessageLimited (DataMsg MCShares) m
       , MessageLimited (DataMsg MCCommitment) m
       , MessageLimited (DataMsg MCOpening) m
       )
    => Logic m
    -> [Relay m]
sscRelays logic =
    [ commitmentRelay (postSscCommitment logic)
    , openingRelay (postSscOpening logic)
    , sharesRelay (postSscShares logic)
    , vssCertRelay (postSscVssCert logic)
    ]

commitmentRelay
    :: ( SscMessageConstraints m
       , MessageLimited (DataMsg MCCommitment) m
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged MCCommitment StakeholderId) MCCommitment m
    -> Relay m
commitmentRelay kv = sscRelay kv

openingRelay
    :: ( SscMessageConstraints m
       , MessageLimited (DataMsg MCOpening) m
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged MCOpening StakeholderId) MCOpening m
    -> Relay m
openingRelay kv = sscRelay kv

sharesRelay
    :: ( SscMessageConstraints m
       , MessageLimited (DataMsg MCShares) m
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged MCShares StakeholderId) MCShares m
    -> Relay m
sharesRelay kv = sscRelay kv

vssCertRelay
    :: ( SscMessageConstraints m
       , MessageLimited (DataMsg MCVssCertificate) m
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate m
    -> Relay m
vssCertRelay kv = sscRelay kv

sscRelay
    :: ( Buildable contents
       , Typeable contents
       , MessageLimited (DataMsg contents) m
       , Bi (DataMsg contents)
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqOrRes (Tagged contents StakeholderId))
       , Message (ReqMsg (Tagged contents StakeholderId))
       , DiffusionWorkMode m
       )
    => KV.KeyVal (Tagged contents StakeholderId) contents m
    -> Relay m
sscRelay kv =
    InvReqData NoMempool $
        InvReqDataParams
          { invReqMsgType = MsgMPC
          , contentsToKey = KV.toKey kv
          , handleInv = \_ -> KV.handleInv kv
          , handleReq = \_ -> KV.handleReq kv
          , handleData = \_ -> KV.handleData kv
          }
