
module Pos.Diffusion.Subscription.Dns
    ( dnsSubscriptionWorker
    ) where

import           Universum

import           Control.Exception (IOException)
import           Data.Either (partitionEithers)
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Data.Time.Units (Millisecond, fromMicroseconds, toMicroseconds)
import           Formatting (int, sformat, shown, (%))
import qualified Network.DNS as DNS
import           System.Wlog (logError, logNotice, logWarning)

import           Mockable (Concurrently, Delay, Mockable, SharedAtomic, SharedAtomicT, delay,
                           forConcurrently, modifySharedAtomic, newSharedAtomic)
import qualified Network.Broadcast.OutboundQueue as OQ
import           Network.Broadcast.OutboundQueue.Types (Alts, peersFromList)

import           Pos.Communication.Protocol (SendActions)
import           Pos.Diffusion.Types (SubscriptionStatus (..))
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
    -> TVar (Map NodeId SubscriptionStatus)
    -> SendActions m
    -> m ()
dnsSubscriptionWorker oq networkCfg DnsDomains{..} keepaliveTimer slotDuration subStatus sendActions = do
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
    void $ forConcurrently allOf (\anyOf -> newMVar (0 :: Millisecond) >>= subscribeAlts dnsPeersVar anyOf)
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
        -> MVar Millisecond
        -> m ()
    subscribeAlts _ (index, []) _ =
        logWarning $ sformat ("dnsSubscriptionWorker: no alternatives given for index "%int) index
    subscribeAlts dnsPeersVar (index, alts) subDuration = do
        -- Any DNSError is squelched. So are IOExceptions, for good measure.
        -- This does not include async exceptions.
        -- It does handle the case in which there's no internet connection, or
        -- a bad configuration, so that the subscription thread will keep on
        -- retrying.
        findAndSubscribe dnsPeersVar subDuration index alts
            `catch` logDNSError
            `catch` logIOException
        d <- swapMVar subDuration 0 >>= retryInterval
        logNotice $ sformat ("dnsSubscriptionWorker: waiting "%int%"ms before trying again")
            (toMicroseconds d `div` 1000)
        delay d
        subscribeAlts dnsPeersVar (index, alts) subDuration

    -- Subscribe to all alternatives, one-at-a-time, until the list is
    -- exhausted.
    subscribeToOne :: MVar Millisecond -> Alts NodeId -> m ()
    subscribeToOne subDuration dnsPeers = case dnsPeers of
        [] -> return ()
        (peer:peers) -> do
            void $ subscribeTo keepaliveTimer slotDuration subStatus subDuration sendActions peer
            subscribeToOne subDuration peers

    -- Resolve a name and subscribe to the node(s) at the addresses.
    findAndSubscribe
        :: SharedAtomicT m (Map Int (Alts NodeId))
        -> MVar Millisecond
        -> Int
        -> Alts (NodeAddr DNS.Domain)
        -> m ()
    findAndSubscribe dnsPeersVar subDuration index alts = do
        -- Resolve all of the names and update the known peers in the queue.
        dnsPeersList <- findDnsPeers index alts
        modifySharedAtomic dnsPeersVar $ \dnsPeers -> do
            let dnsPeers' = M.insert index dnsPeersList dnsPeers
            void $ liftIO $ OQ.updatePeersBucket oq BucketBehindNatWorker $ \_ ->
                peersFromList mempty ((,) NodeRelay <$> M.elems dnsPeers')
            pure (dnsPeers', ())
        -- Try to subscribe to some peer.
        -- If they all fail, wait a while before trying again.
        subscribeToOne subDuration dnsPeersList

    logIOException :: IOException -> m ()
    logIOException ioException =
        logError $ sformat ("dnsSubscriptionWorker: "%shown) ioException

    logDNSError :: DNS.DNSError -> m ()
    logDNSError dnsError =
        logError $ sformat ("dnsSubscriptionWorker: "%shown) dnsError

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
    --
    -- The formula to calculate the retry interval is
    -- @
    -- retryInterval = floor (slotDuration / 2 ^ (number of slots that passed during the longest subscription duration))
    -- @
    -- For a @20s@ slot: the values will be @20s@, @10s@, @5s@, @2s@,
    -- @1s@, @0s@, i.e. if there was a subscription that lasted more than
    -- @5@ slots we will try to re-subscribe immediately.
    retryInterval :: Millisecond -> m Millisecond
    retryInterval d = do
        slotDur <- slotDuration
        let -- slot duration in microseconds
            slotDurF :: Float
            slotDurF = fromIntegral $ toMicroseconds slotDur
            -- Number of slots that passed in the longest subscription duration
            slotsF :: Float
            slotsF = (fromIntegral $ toMicroseconds d) / slotDurF
        return $ fromMicroseconds $ floor $ slotDurF / (2 ** slotsF)

    msgDnsFailure :: Int -> [DNS.DNSError] -> Text
    msgDnsFailure = sformat ("dnsSubscriptionWorker: DNS failure for index "%int%": "%shown)

    msgNoRelays :: Int -> Text
    msgNoRelays = sformat ("dnsSubscriptionWorker: no relays found for index "%int)

{-# ANN dnsSubscriptionWorker ("HLint: ignore Use unless" :: String) #-}
