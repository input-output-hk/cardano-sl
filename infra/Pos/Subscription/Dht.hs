
module Pos.Subscription.Dht
    ( dhtSubscriptionWorker
    ) where

import qualified Control.Concurrent.STM as STM
import           Universum

import           Formatting (sformat, shown, (%))
import           Network.Broadcast.OutboundQueue.Types (Peers)
import           System.Wlog (logNotice)

import           Pos.Communication.Protocol (NodeId, Worker)
import           Pos.DHT.Real.Real (kademliaGetKnownPeers)
import           Pos.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.DHT.Workers (DhtWorkMode)
import           Pos.KnownPeers (MonadKnownPeers (..))
import           Pos.Network.Types (Bucket (..), NodeType, choosePeers)
import           Pos.Util.TimeWarp (addressToNodeId)


-- | This worker will update the known peers (via MonadKnownPeers) every time
-- the Kademlia peers change.
dhtSubscriptionWorker
    :: forall ctx m .
       ( DhtWorkMode ctx m
       )
    => KademliaDHTInstance
    -> NodeType
    -> Int -- ^ valency
    -> Int -- ^ fallbacks
    -> Worker m
dhtSubscriptionWorker kademliaInst peerType valency fallbacks _sendActions = do
    logNotice "Kademlia subscription worker started"
    updateForeverNoSubscribe mempty
  where
    updateForeverNoSubscribe
        :: Peers NodeId
        -> m ()
    updateForeverNoSubscribe peers = do
        -- Will block until at least one of the current best peers changes.
        peers' <- atomically $ updateFromKademliaNoSubscribe peers
        logNotice $
            sformat ("Kademlia peer set changed to "%shown) peers'
        void $ updatePeersBucket BucketKademliaWorker (const peers')
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
