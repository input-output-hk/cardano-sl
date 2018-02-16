
module Pos.Diffusion.Subscription.Dns
    ( dnsSubscriptionWorker
    ) where

import           Control.Exception.Safe (IOException, tryIO)
import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as M
import           Data.Time.Units (Millisecond, Second, convertUnit)
import           Formatting (int, sformat, shown, (%))
import qualified Network.DNS as DNS
import           System.Wlog (logError, logNotice, logWarning)
import           Universum

import           Mockable (Concurrently, Delay, Mockable, SharedAtomic, SharedAtomicT, delay,
                           forConcurrently, modifySharedAtomic, newSharedAtomic)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (Alts, peersFromList)

import           Pos.Communication.Protocol (SendActions)
import           Pos.Diffusion.Subscription.Common
import           Pos.Network.DnsDomains (NodeAddr)
import           Pos.Network.Types (Bucket (..), DnsDomains (..), NetworkConfig (..), NodeId (..),
                                    NodeType (..), resolveDnsDomains)
import           Pos.Util.Timer (Timer)

dnsSubscriptionWorker
    :: forall pack kademlia m.
     ( SubscriptionMode m
     , Mockable Delay m
     , Mockable SharedAtomic m
     , Mockable Concurrently m
     )
    => OQ.OutboundQ pack NodeId Bucket
    -> NetworkConfig kademlia
    -> DnsDomains DNS.Domain
    -> Timer
    -> m Millisecond
    -> SendActions m
    -> m ()
dnsSubscriptionWorker oq networkCfg DnsDomains{..} keepaliveTimer nextSlotDuration sendActions = do
    -- Shared state between the threads which do subscriptions.
    -- It's a 'Map Int (Alts NodeId)' used to determine the current
    -- peers set for our bucket 'BucketBehindNatWorker'. Each thread takes
    -- care of its own index and updates the peers while holding the lock, so
    -- that the threads don't erase each-others' work.
    let initialDnsPeers :: Map Int (Alts NodeId)
        initialDnsPeers = M.fromList $ map (\(i, _) -> (i, [])) allOf
    dnsPeersVar <- newSharedAtomic initialDnsPeers
    -- There's a thread for each conjunct which attempts to subscribe to one of
    -- the alternatives.
    -- This gives valency and fallbacks implicitly, just as for static
    -- routes. Valency is the length of the outer list (conjuncts) and
    -- fallbacks (for a given outer list element) is the length of the inner
    -- list (disjuncts).
    logNotice $ sformat ("dnsSubscriptionWorker: valency "%int) (length allOf)
    void $ forConcurrently allOf (subscribeAlts dnsPeersVar)
    logNotice $ sformat ("dnsSubscriptionWorker: all "%int%" threads finished") (length allOf)
  where

    allOf :: [(Int, Alts (NodeAddr DNS.Domain))]
    allOf = zip [1..] dnsDomains

    -- Resolve all of the names and try to subscribe to one.
    -- If a subscription goes down, try later names.
    -- When the list is exhausted (either because it's empty to begin with, or
    -- because all subscriptions to have failed), wait a while before retrying
    -- (see 'retryInterval').
    subscribeAlts
        :: SharedAtomicT m (Map Int (Alts NodeId))
        -> (Int, Alts (NodeAddr DNS.Domain))
        -> m ()
    subscribeAlts _ (index, []) =
        logWarning $ sformat ("dnsSubscriptionWorker: no alternatives given for index "%int) index
    subscribeAlts dnsPeersVar (index, alts) = do
        -- Resolve all of the names and update the known peers in the queue.
        mDnsPeersList <- findDnsPeers index alts
        case mDnsPeersList of
            Left ioexception -> do
                logError $ sformat ("dnsSubscriptionWorker: failed to resolve names "%shown) ioexception
            Right dnsPeersList -> do
                modifySharedAtomic dnsPeersVar $ \dnsPeers -> do
                    let dnsPeers' = M.insert index dnsPeersList dnsPeers
                    void $ OQ.updatePeersBucket oq BucketBehindNatWorker $ \_ ->
                        peersFromList mempty ((,) NodeRelay <$> M.elems dnsPeers')
                    pure (dnsPeers', ())
                -- Try to subscribe to some peer.
                -- If they all fail, wait a while before trying again.
                subscribeToOne dnsPeersList
        retryInterval >>= delay
        subscribeAlts dnsPeersVar (index, alts)

    subscribeToOne :: Alts NodeId -> m ()
    subscribeToOne dnsPeers = case dnsPeers of
        [] -> return ()
        (peer:peers) -> do
            void $ subscribeTo keepaliveTimer sendActions peer
            subscribeToOne peers

    -- Find peers via DNS, preserving order.
    -- In case multiple addresses are returned for one name, they're flattened
    -- and we forget the boundaries, but all of the addresses for a given name
    -- are adjacent.
    -- An IOException from resolveDnsDomains is squelched.
    -- It's probably (certainly?) a network-related problem, like no
    -- internet connection, in which case we don't want the subscription
    -- thread to blow up.
    -- NB: safe-exceptions package is used, so this doesn't catch async
    -- exceptions.
    findDnsPeers
        :: Int
        -> Alts (NodeAddr DNS.Domain)
        -> m (Either IOException (Alts NodeId))
    findDnsPeers index alts = do
        mNodeIds <- liftIO $ tryIO $ resolveDnsDomains networkCfg alts
        case mNodeIds of
            Left ioexception -> pure (Left ioexception)
            Right nodeIds -> do
                let (errs, nids_) = partitionEithers nodeIds
                    nids = mconcat nids_
                when (null nids)       $ logError (msgNoRelays index)
                when (not (null errs)) $ logError (msgDnsFailure index errs)
                return (Right nids)

    -- How long to wait before retrying in case no alternative can be
    -- subscribed to.
    retryInterval :: m Millisecond
    retryInterval = do
        slotDur <- nextSlotDuration
        pure $ max (slotDur `div` 4) (convertUnit (5 :: Second))

    msgDnsFailure :: Int -> [DNS.DNSError] -> Text
    msgDnsFailure = sformat ("dnsSubscriptionWorker: DNS failure for index "%int%": "%shown)

    msgNoRelays :: Int -> Text
    msgNoRelays = sformat ("dnsSubscriptionWorker: no relays found for index "%int)

{-# ANN dnsSubscriptionWorker ("HLint: ignore Use unless" :: String) #-}
