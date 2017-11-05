
module Pos.Subscription.Dns
    ( dnsSubscriptionWorker
    ) where

import           Data.Either                           (partitionEithers)
import qualified Data.Map.Strict                       as M
import           Data.Time.Units                       (Millisecond, Second, convertUnit)
import           Formatting                            (sformat, shown, int, (%))
import qualified Network.DNS                           as DNS
import           System.Wlog                           (logError, logNotice, logWarning)
import           Universum

import           Mockable                              (Delay, Mockable, delay,
                                                        Concurrently,
                                                        forConcurrently,
                                                        SharedAtomic,
                                                        SharedAtomicT,
                                                        newSharedAtomic,
                                                        modifySharedAtomic)
import           Network.Broadcast.OutboundQueue.Types (peersFromList, Alts)

import           Pos.Communication.Protocol            (Worker)
import           Pos.KnownPeers                        (MonadKnownPeers (..))
import           Pos.Network.DnsDomains                (NodeAddr)
import           Pos.Network.Types                     (Bucket (..), DnsDomains (..),
                                                        NetworkConfig (..),
                                                        NodeId (..), NodeType (..),
                                                        resolveDnsDomains)
import           Pos.Slotting                          (MonadSlotsData,
                                                        getNextEpochSlotDuration)
import           Pos.Subscription.Common
import           Pos.Util.Timer                        (Timer)

dnsSubscriptionWorker
    :: forall kademlia ctx m.
     ( SubscriptionMode m
     , MonadSlotsData ctx m
     , Mockable Delay m
     , Mockable SharedAtomic m
     , Mockable Concurrently m
     )
    => NetworkConfig kademlia
    -> DnsDomains DNS.Domain
    -> Timer
    -> Worker m
dnsSubscriptionWorker networkCfg DnsDomains{..} keepAliveTimer sendActions = do
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
        dnsPeersList <- findDnsPeers index alts
        modifySharedAtomic dnsPeersVar $ \dnsPeers -> do
            let dnsPeers' = M.insert index dnsPeersList dnsPeers
            void $ updatePeersBucket BucketBehindNatWorker $ \_ ->
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
            void $ subscribeTo keepAliveTimer sendActions peer
            subscribeToOne peers

    -- Find peers via DNS, preserving order.
    -- In case multiple addresses are returned for one name, they're flattened
    -- and we forget the boundaries, but all of the addresses for a given name
    -- are adjacent.
    findDnsPeers :: Int -> Alts (NodeAddr DNS.Domain) -> m (Alts NodeId)
    findDnsPeers index alts = do
        mNodeIds <- liftIO $ resolveDnsDomains networkCfg alts
        let (errs, nids_) = partitionEithers mNodeIds
            nids = mconcat nids_
        when (null nids)       $ logError (msgNoRelays index)
        when (not (null errs)) $ logError (msgDnsFailure index errs)
        return nids

    -- How long to wait before retrying in case no alternative can be
    -- subscribed to.
    retryInterval :: m Millisecond
    retryInterval = do
        slotDur <- getNextEpochSlotDuration
        pure $ max (slotDur `div` 4) (convertUnit (5 :: Second))

    msgDnsFailure :: Int -> [DNS.DNSError] -> Text
    msgDnsFailure = sformat ("dnsSubscriptionWorker: DNS failure for index "%int%": "%shown)

    msgNoRelays :: Int -> Text
    msgNoRelays = sformat ("dnsSubscriptionWorker: no relays found for index "%int)

{-# ANN dnsSubscriptionWorker ("HLint: ignore Use unless" :: String) #-}
