
module Pos.Diffusion.Subscription.Dht
    ( dhtSubscriptionWorker
    ) where

import qualified Control.Concurrent.STM as STM
import           Universum

import           Formatting (sformat, shown, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (Peers)
import           System.Wlog (WithLogger, logNotice)

import           Pos.Communication.Protocol (NodeId, Worker)
import           Pos.DHT.Real.Real (kademliaGetKnownPeers)
import           Pos.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.Infra.Configuration (HasInfraConfiguration)
import           Pos.Network.Types (Bucket (..), NodeType, choosePeers)
import           Pos.Util.TimeWarp (addressToNodeId)


-- | This worker will update the known peers (via MonadKnownPeers) every time
-- the Kademlia peers change.
dhtSubscriptionWorker
    :: forall pack m .
       ( MonadIO m
       , WithLogger m
       , HasInfraConfiguration
       )
    => OQ.OutboundQ pack NodeId Bucket
    -> KademliaDHTInstance
    -> NodeType
    -> Int -- ^ valency
    -> Int -- ^ fallbacks
    -> Worker m
dhtSubscriptionWorker oq kademliaInst peerType valency fallbacks _sendActions = do
    logNotice "Kademlia subscription worker started"
    updateForeverNoSubscribe mempty
    {-
    -- A  TMVar NodeId  for each peer to which
    toSubscribe <- liftIO . atomically $ forM [1..valency] (const STM.newEmptyTMVar)
    -- A  TVar (Set NodeId)  for the set of peers to which we're currently
    -- subscribed.
    subscribed <- liftIO $ STM.newTVarIO S.empty
    -- Spawn a thread for each of the subscriptions that we wish to make.
    withAsyncs (fmap (subThread sendActions subscribed) toSubscribe) $ do
        -- Repeatedly do an STM transaction which retries until the best set
        -- of peers changes. Read the where clause comments and program text
        -- for more details.
        updateForever subscribed toSubscribe mempty
    -}
  where

    {-
    -- Spawn a bunch of threads using withAsync and ignore their results.
    -- They'll be cancelled when the continuation finishes.
    withAsyncs :: [m x] -> m t -> m t
    withAsyncs [] k = k
    withAsyncs (it : rest) k = withAsync it $ const (withAsyncs rest k)


    -- Subscribe to the node which appears in the TMVar, updating the TVar so
    -- that it includes that node whenever a subscription is active.
    subThread :: SendActions m -> STM.TVar (Set NodeId) -> STM.TMVar NodeId -> m ()
    subThread sendActions subscribedVar peerVar =
        -- Block until we get a NodeId to subscribe to.
        peer <- liftIO . atomically $ STM.readTMVar peerVar
        subscribeTo sendActions peer
        liftIO . atomically $ do
            -- Take the TMVar so that the updating thread can replace it
            -- with another candidate, whenever it has one.
            STM.takeTMVar peerVar
            STM.modifyTVar subscribedVar (S.delete peer)
        action

    -- TODO import this and re-use in the behind-NAT subscription worker.

    subscribeTo :: SendActions m -> NodeId -> m ()
    subscribeTo sendActions peer = do
        logNotice $ sformat ("Establishing subscription to "%shown) peer
        Left (ex :: SomeException) <- try $ withConnectionTo sendActions peer $ \_ ->
            pure $ Conversation $ \conv -> do
                send conv MsgSubscribe
                logNotice $ sformat ("Subscription to "%shown%" established") peer
                _void :: Maybe Void <- recv conv 0 -- Other side will never send
                return _void
        logNotice $ sformat ("Subscription to "%shown%" terminated: "%shown) peer ex

    -- Main updating action. It runs an STM transaction until the new Peers
    -- term comes out. When this happens depends upon
    --   1. The Kadmelia internal state TVar.
    --   2. The state of active subscriptions, as reflected by the TMVars
    --      in the 'peerVars' parameters.
    -- See 'updateFromKademlia'. It guarantees that the 'Peers NodeId' term
    -- it produces is not the same as the one it takes as its final parameter
    -- (if they're equal, it will retry).
    --
    -- It is assumed that there is no concurrent user of the MonadKnownPeers
    -- mutator functions (we'll overwrite them).
    updateForever
        :: STM.TVar (Set NodeId)
        -> [STM.TMVar NodeId]
        -> Peers NodeId
        -> m ()
    updateForever subscribedVar peerVars peers = do
        -- Will block until at least one of the current best peers changes.
        peers' <- liftIO . atomically $ updateFromKademlia subscribedVar peerVars peers
        logNotice $
            sformat ("Kademlia peer set changed to "%shown) peers'
        updateKnownPeers (const peers')
        updateForever subscribedVar peerVars peers'

    -- Given a list of 'TMVar's corresponding to subscription threads, try to
    -- put a list of 'NodeId's into them. Any full ones are passed up, and
    -- the 'NodeId' is carried on to the next one. This is the second component
    -- of the tuple. The first component is an accumulation of the 'NodeId's
    -- that are actually in the 'TMVar's.
    updatePeerVars
        :: STM.TVar (Set NodeId)
        -> [STM.TMVar NodeId]
        -> ([NodeId], [NodeId])
        -> STM ([NodeId], [NodeId])
    updatePeerVars _ [] (mains, remaining) = return (reverse mains, remaining)
    updatePeerVars subscribedVar (peerVar : peerVars) (mains, []) = do
        -- There's nothing to put into this TMVar.
        -- If it's full, tack its value onto the list of nodes present.
        current <- STM.tryReadTMVar peerVar
        let mains' = maybe identity (:) current mains
        updatePeerVars subscribedVar peerVars (mains', [])
    updatePeerVars subscribedVar (peerVar : peerVars) (mains, peer : peers) = do
        current <- STM.tryReadTMVar peerVar
        (mains', peers') <- case current of
            -- It's empty. Put the new one in.
            Nothing -> do
                STM.putTMVar peerVar peer
                STM.modifyTVar subscribedVar (S.insert peer)
                return (peer : mains, peers)
            -- It's not empty. Carry the new peer forward and remember the
            -- current peer.
            Just peer' -> return (peer' : mains, peer : peers)
        updatePeerVars subscribedVar peerVars (mains', peers')

    updateFromKademlia
        :: STM.TVar (Set NodeId)
        -> [STM.TMVar NodeId]
        -> Peers NodeId
        -> STM (Peers NodeId)
    updateFromKademlia subscribedVar peerVars oldPeers = do
        addrList <- kademliaGetKnownPeers kademliaInst
        alreadySubscribed <- STM.readTVar subscribedVar
        let peersList = fmap addressToNodeId addrList
            uncontactedPeers = S.toList (S.fromList peersList S.\\ alreadySubscribed)
        -- Try to put the peers which are not already in the 'peerVars' into
        -- one of them. The result: a list of the ones which *are* in 'peerVars'
        -- along with the ones from the original list 'uncontactedPeers' which
        -- didn't make it in.
        (mains, remaining) <- updatePeerVars subscribedVar peerVars ([], uncontactedPeers)
        -- The ones which did make it into one of the 'peerVars' (or were
        -- already there) become the mains, and any remainders become a
        -- fallback.
        let peersList' = zipWith (:) mains (fmap pure remaining ++ repeat [])
            newPeers = peersFromList (fmap ((,) peerType) peersList')
        when (newPeers == oldPeers) STM.retry
        return newPeers
    -}

    updateForeverNoSubscribe
        :: Peers NodeId
        -> m ()
    updateForeverNoSubscribe peers = do
        -- Will block until at least one of the current best peers changes.
        peers' <- atomically $ updateFromKademliaNoSubscribe peers
        logNotice $
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
