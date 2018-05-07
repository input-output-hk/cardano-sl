{-# LANGUAGE RankNTypes #-}

-- | Send and receive for delegation.

module Pos.Diffusion.Full.Delegation
       ( delegationListeners
       , delegationOutSpecs
       , sendPskHeavy
       ) where

import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ

import           Pos.Binary ()
import           Pos.Communication.Limits (mlHeavyDlgIndex, mlProxySecretKey)
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (MsgType (..), NodeId, EnqueueMsg,
                                             MkListeners, OutSpecs)
import           Pos.Communication.Relay (DataParams (..), Relay (..),
                                          relayListeners, dataFlow,
                                          relayPropagateOut)
import           Pos.Core       (ProxySKHeavy)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Logic.Types (Logic (..))
import           Pos.Network.Types (Bucket)

delegationListeners
    :: ( DiffusionWorkMode m )
    => Logic m
    -> OQ.OutboundQ pack NodeId Bucket
    -> EnqueueMsg m
    -> MkListeners m
delegationListeners logic oq enqueue = relayListeners oq enqueue (delegationRelays logic)

-- | Listeners for requests related to delegation processing.
delegationRelays
    :: forall m .
       ( DiffusionWorkMode m )
    => Logic m
    -> [Relay m]
delegationRelays logic = [ pskHeavyRelay logic ]

-- | 'OutSpecs' for the tx relays, to keep up with the 'InSpecs'/'OutSpecs'
-- motif required for communication.
-- The 'Logic m' isn't *really* needed, it's just an artefact of the design.
delegationOutSpecs
    :: forall m .
       ( DiffusionWorkMode m
       )
    => Logic m
    -> OutSpecs
delegationOutSpecs logic = relayPropagateOut (delegationRelays logic)

pskHeavyRelay
    :: ( DiffusionWorkMode m )
    => Logic m
    -> Relay m
pskHeavyRelay logic = Data $ DataParams
    MsgTransaction
    (\_ _ -> postPskHeavy logic)
    -- The message size limit for ProxySKHeavy: a ProxySecretKey with an
    -- EpochIndex.
    (pure (mlProxySecretKey mlHeavyDlgIndex))

sendPskHeavy
    :: forall m .
       ( DiffusionWorkMode m )
    => EnqueueMsg m
    -> ProxySKHeavy
    -> m ()
sendPskHeavy enqueue = dataFlow "pskHeavy" enqueue (MsgTransaction OQ.OriginSender)
