{-# LANGUAGE RankNTypes #-}

-- | Send and receive for delegation.

module Pos.Diffusion.Full.Delegation
       ( delegationListeners
       , sendPskHeavy
       ) where

import           Universum

import qualified Network.Broadcast.OutboundQueue as OQ

import           Pos.Binary ()
import           Pos.Communication.Limits ()
import           Pos.Communication.Message ()
import           Pos.Communication.Protocol (MsgType (..), NodeId, EnqueueMsg, MkListeners)
import           Pos.Communication.Relay (DataParams (..), Relay (..),
                                          relayListeners, dataFlow)
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

pskHeavyRelay
    :: ( DiffusionWorkMode m )
    => Logic m
    -> Relay m
pskHeavyRelay logic = Data $ DataParams MsgTransaction $ \_ _ -> postPskHeavy logic

sendPskHeavy
    :: forall m .
       ( DiffusionWorkMode m )
    => EnqueueMsg m
    -> ProxySKHeavy
    -> m ()
sendPskHeavy enqueue = dataFlow "pskHeavy" enqueue (MsgTransaction OQ.OriginSender)
