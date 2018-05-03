{-# LANGUAGE RankNTypes #-}

module Pos.Diffusion.Full.Ssc
    ( sscListeners
    , sscOutSpecs
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
-- Message instances for various types.
-- TODO should move these into the Diffusion module subtree.
import           Pos.Communication.Message ()
import           Pos.Communication.Limits (Limit, mlMCOpening, mlMCVssCertificate,
                                           mlMCCommitment, mlMCShares)
import           Pos.Communication.Relay (DataMsg, InvOrData, InvReqDataParams (..),
                                          MempoolParams (NoMempool), Relay (..), ReqMsg, ReqOrRes,
                                          relayListeners, relayPropagateOut)
import           Pos.Communication.Types.Protocol (MsgType (..), NodeId, EnqueueMsg,
                                                   MkListeners, OutSpecs)
import           Pos.Core (StakeholderId)
import           Pos.Network.Types (Bucket)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Ssc.Message (MCCommitment (..), MCOpening (..), MCShares (..),
                                  MCVssCertificate (..), SscMessageConstraints)
import           Pos.Util.Trace (Trace, Severity)

sscListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
sscListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (sscRelays logic)

sscRelays
    :: ( SscMessageConstraints )
    => Logic IO
    -> [Relay]
sscRelays logic =
    [ commitmentRelay logic (postSscCommitment logic)
    , openingRelay (postSscOpening logic)
    , sharesRelay logic (postSscShares logic)
    , vssCertRelay (postSscVssCert logic)
    ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
sscOutSpecs :: Logic IO -> OutSpecs
sscOutSpecs logic = relayPropagateOut (sscRelays logic)

commitmentRelay
    :: ( SscMessageConstraints )
    => Logic IO
    -> KV.KeyVal (Tagged MCCommitment StakeholderId) MCCommitment IO
    -> Relay
commitmentRelay logic kv = sscRelay kv (mlMCCommitment <$> getAdoptedBVData logic)

openingRelay
    :: ( SscMessageConstraints )
    => KV.KeyVal (Tagged MCOpening StakeholderId) MCOpening IO
    -> Relay
openingRelay kv = sscRelay kv (pure mlMCOpening)

sharesRelay
    :: ( SscMessageConstraints
       )
    => Logic IO
    -> KV.KeyVal (Tagged MCShares StakeholderId) MCShares IO
    -> Relay
sharesRelay logic kv = sscRelay kv (mlMCShares <$> getAdoptedBVData logic)

vssCertRelay
    :: ( SscMessageConstraints )
    => KV.KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate IO
    -> Relay
vssCertRelay kv = sscRelay kv (pure mlMCVssCertificate)

sscRelay
    :: ( Buildable contents
       , Typeable contents
       , Bi (DataMsg contents)
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqOrRes (Tagged contents StakeholderId))
       , Message (ReqMsg (Tagged contents StakeholderId))
       )
    => KV.KeyVal (Tagged contents StakeholderId) contents IO
    -> IO (Limit contents)
    -> Relay
sscRelay kv mkLimit =
    InvReqData NoMempool $
        InvReqDataParams
          { invReqMsgType = MsgMPC
          , contentsToKey = KV.toKey kv
          , handleInv = \_ -> KV.handleInv kv
          , handleReq = \_ -> KV.handleReq kv
          , handleData = \_ -> KV.handleData kv
          , irdpMkLimit = mkLimit
          }
