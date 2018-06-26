
module Pos.Infra.Diffusion.Subscription.Dht
    ( dhtSubscriptionWorker
    ) where

import qualified Control.Concurrent.STM as STM
import           Universum

import           Formatting (sformat, shown, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (Peers)

import           Pos.Infra.Communication.Protocol (NodeId, SendActions)
import           Pos.Infra.DHT.Real.Real (kademliaGetKnownPeers)
import           Pos.Infra.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.Infra.Network.Types (Bucket (..), NodeType, choosePeers)
import           Pos.Infra.Util.TimeWarp (addressToNodeId)
import           Pos.Util.Trace (Severity (..), Trace, traceWith)


-- | This worker will update the known peers every time the Kademlia peers
-- change.
dhtSubscriptionWorker
    :: forall pack .
       Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> KademliaDHTInstance
    -> NodeType
    -> Int -- ^ valency
    -> Int -- ^ fallbacks
    -> SendActions
    -> IO ()
dhtSubscriptionWorker logTrace oq kademliaInst peerType valency fallbacks _sendActions = do
    traceWith logTrace (Notice, "Kademlia subscription worker started")
    updateForeverNoSubscribe mempty
  where
    updateForeverNoSubscribe
        :: Peers NodeId
        -> IO ()
    updateForeverNoSubscribe peers = do
        -- Will block until at least one of the current best peers changes.
        peers' <- atomically $ updateFromKademliaNoSubscribe peers
        traceWith logTrace $ ((,) Notice) $
            sformat ("Kademlia peer set changed to "%shown) peers'
        void $ OQ.updatePeersBucket oq BucketKademliaWorker (const peers')
        updateForeverNoSubscribe peers'

    updateFromKademliaNoSubscribe
        :: Peers NodeId
        -> STM (Peers NodeId)
    updateFromKademliaNoSubscribe oldPeers = do
        -- This does a TVar read. Changes to that TVar will wake us up if we
        -- retry.
        addrList <- kademliaGetKnownPeers kademliaInst
        let peersList = fmap addressToNodeId addrList
            newPeers = choosePeers valency fallbacks peerType peersList
        when (newPeers == oldPeers) STM.retry
        return newPeers
