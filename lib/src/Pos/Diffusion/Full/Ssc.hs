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
import           Pos.Binary.Limit (Limit)
-- Message instances for various types.
-- TODO should move these into the Diffusion module subtree.
import           Pos.Chain.Ssc (MCCommitment (..), MCOpening (..),
                     MCShares (..), MCVssCertificate (..))
import           Pos.Chain.Update (ConsensusEra (..), consensusEraBVD)
import           Pos.Communication.Limits (mlMCCommitment, mlMCOpening,
                     mlMCShares, mlMCVssCertificate)
import           Pos.Core (StakeholderId)
import           Pos.Infra.Communication.Relay (DataMsg, InvOrData,
                     InvReqDataParams (..), MempoolParams (NoMempool),
                     Relay (..), ReqMsg, ReqOrRes, relayListeners,
                     relayPropagateOut)
import           Pos.Infra.Communication.Types.Protocol (EnqueueMsg,
                     MkListeners, MsgType (..), NodeId, OutSpecs)
import           Pos.Infra.Network.Types (Bucket)
import           Pos.Logic.Types (Logic (..))
import qualified Pos.Logic.Types as KV (KeyVal (..))
import           Pos.Util.Trace (Severity, Trace)

sscListeners
    :: Trace IO (Severity, Text)
    -> Logic IO
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg
    -> MkListeners
sscListeners logTrace logic oq enqueue = relayListeners logTrace oq enqueue (sscRelays logic)

sscRelays
    :: Logic IO
    -> [Relay]
sscRelays logic =
    [ commitmentRelay logic (postSscCommitment logic)
    , openingRelay logic (postSscOpening logic)
    , sharesRelay logic (postSscShares logic)
    , vssCertRelay logic (postSscVssCert logic)
    ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
sscOutSpecs :: Logic IO -> OutSpecs
sscOutSpecs logic = relayPropagateOut (sscRelays logic)

commitmentRelay
    :: Logic IO
    -> KV.KeyVal (Tagged MCCommitment StakeholderId) MCCommitment IO
    -> Relay
commitmentRelay logic kv = sscRelay logic kv (mlMCCommitment <$> getAdoptedBVData logic)

openingRelay
    :: Logic IO
    -> KV.KeyVal (Tagged MCOpening StakeholderId) MCOpening IO
    -> Relay
openingRelay logic kv = sscRelay logic kv (pure mlMCOpening)

sharesRelay
    :: Logic IO
    -> KV.KeyVal (Tagged MCShares StakeholderId) MCShares IO
    -> Relay
sharesRelay logic kv = sscRelay logic kv (mlMCShares <$> getAdoptedBVData logic)

vssCertRelay
    :: Logic IO
    -> KV.KeyVal (Tagged MCVssCertificate StakeholderId) MCVssCertificate IO
    -> Relay
vssCertRelay logic kv = sscRelay logic kv (pure mlMCVssCertificate)

sscRelay
    :: ( Buildable contents
       , Typeable contents
       , Bi (DataMsg contents)
       , Message (InvOrData (Tagged contents StakeholderId) contents)
       , Message (ReqOrRes (Tagged contents StakeholderId))
       , Message (ReqMsg (Tagged contents StakeholderId))
       )
    => Logic IO
    -> KV.KeyVal (Tagged contents StakeholderId) contents IO
    -> IO (Limit contents)
    -> Relay
sscRelay logic kv mkLimit =
    InvReqData NoMempool $
        InvReqDataParams
          { invReqMsgType = MsgMPC
          , contentsToKey = KV.toKey kv
          , handleInv = \_ x -> ifObft False $ (KV.handleInv kv) x
          , handleReq = \_ x -> ifObft Nothing $ (KV.handleReq kv) x
          , handleData = \_ x -> ifObft False $ (KV.handleData kv) x
          , irdpMkLimit = mkLimit
          }
  where
    ifObft :: a -> IO a -> IO a
    ifObft d action = do
        era <- consensusEraBVD <$> (getAdoptedBVData logic)
        case era of
            OBFT _ -> pure d
            _      -> action
