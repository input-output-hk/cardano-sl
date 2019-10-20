{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}

-- | 'SubscriptionTarget' backed by DNS.

module Pos.Infra.Diffusion.Subscription.Dns
    ( dnsSubscriptionTarget
    , dnsSubscriptionWorker
    , retryInterval
    ) where

import           Universum hiding (atomically, set)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.MVar (modifyMVar, modifyMVar_)
import           Control.Concurrent.STM (atomically)
import           Control.Exception (IOException, SomeException, toException)
import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import           Data.Set (Set)
import qualified Data.Set as Set
import           Data.Time.Units (Microsecond, Millisecond, convertUnit,
                     fromMicroseconds, toMicroseconds)
import           Formatting (int, sformat, shown, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.DNS as DNS

import           Pos.Infra.Communication.Protocol (SendActions)
import           Pos.Infra.Diffusion.Subscription.Common (networkSubscribeTo')
import           Pos.Infra.Diffusion.Subscription.Status (SubscriptionStates)
import           Pos.Infra.Diffusion.Subscription.Subscriber (SubscribeTo,
                     SubscriptionTarget (..), subscriber)
import           Pos.Infra.Network.DnsDomains (NodeAddr)
import           Pos.Infra.Network.Types (Bucket (..), DnsDomains (..),
                     NodeId (..), NodeType (..), resolveDnsDomains)
import           Pos.Util.Timer (Timer, newTimer, startTimer, waitTimer)
import           Pos.Util.Trace (Severity (..), Trace, contramap, traceWith)

-- | Resolve a fixed list of names to a NodeId (using a given port) repeatedly.
-- If the resolution may give more than one address, the addresses are given
-- in-order, and a new resolution will take place once they have all been
-- consumed.
dnsSubscriptionTarget
    :: Trace IO (Severity, Text)
    -> IO Microsecond -- ^ How long to wait if a DNS resolution fails
                      -- (IOError or DNSError).
    -> Word16 -- ^ The port to use, to construct a 'NodeId'.
    -> [NodeAddr DNS.Domain] -- ^ Domain names to resolve
    -> SubscriptionTarget IO NodeId
dnsSubscriptionTarget logTrace timeoutError defaultPort addrs =
    SubscriptionTarget (resolve >>= listTargets)

  where

    listTargets :: [[NodeId]] -> IO (Maybe (NodeId, SubscriptionTarget IO NodeId))
    listTargets [] = do
      -- Wait 30s before another round; otherwise the node will exhaust
      -- available opened file descriptors.
      threadDelay 30000000
      getSubscriptionTarget (dnsSubscriptionTarget logTrace timeoutError defaultPort addrs)
    listTargets ([] : fallbacks)                 = listTargets fallbacks
    listTargets ((nodeId : nodeIds) : fallbacks) = pure (Just (nodeId, SubscriptionTarget (listTargets (nodeIds : fallbacks))))

    logTraceError :: Trace IO Text
    logTraceError = contramap ((,) Error) logTrace

    -- Resolve the DNS domains to a list of fallbacks: each list corresponds to
    -- one domain name and later lists are fallbacks in case earlier lists
    -- are exhausted.
    resolve :: IO [[NodeId]]
    resolve = do
        outcome <- fmap Right (resolveOne logTraceError defaultPort addrs)
            `catch` handleDnsError
            `catch` handleIoError
        case outcome of
            Left err -> do
                -- If nothing was resolved, wait a while before trying again.
                traceWith logTrace (Error, formatErr err)
                timeoutError >>= threadDelay . fromIntegral
                resolve
            Right nodeIds -> pure nodeIds

    handleDnsError :: DNS.DNSError -> IO (Either SomeException t)
    handleDnsError = return . Left . toException

    handleIoError :: IOException -> IO (Either SomeException t)
    handleIoError = return . Left . toException

    formatErr :: SomeException -> Text
    formatErr err = sformat ("exception while resolving domains "%shown%": "%shown) addrs err

-- | Find peers via DNS, preserving order.
-- It's possible for multiple addresses to be returned for one name, so they
-- are returned in a list.
-- Any failures are traced here. Also, if none of the names resolved to any
-- addresses (the concatenation of the resulting list of lists is empty) that
-- will be traced as well.
resolveOne
    :: Trace IO Text
    -> Word16
    -> [NodeAddr DNS.Domain]
    -> IO [[NodeId]]
resolveOne logTrace defaultPort nodeAddrs = do
    mNodeIds <- resolveDnsDomains defaultPort nodeAddrs
    let (errs, nids_) = partitionEithers mNodeIds
        nids = mconcat nids_
    when (null nids)   $ traceWith logTrace msgNoRelays
    unless (null errs) $ traceWith logTrace (msgDnsFailure errs)
    return nids_

  where

    msgDnsFailure :: [DNS.DNSError] -> Text
    msgDnsFailure = sformat ("DNS resolution failure for "%shown%": "%shown) nodeAddrs

    msgNoRelays :: Text
    msgNoRelays = sformat ("DNS resolution for "%shown%" gave no results") nodeAddrs

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
retryInterval :: Millisecond -> Millisecond -> Millisecond
retryInterval d slotDur =
    let -- slot duration in microseconds
        slotDurF :: Float
        slotDurF = fromIntegral $ toMicroseconds slotDur
        -- Number of slots that passed in the longest subscription duration
        slotsF :: Float
        slotsF = (fromIntegral $ toMicroseconds d) / slotDurF
    in  fromMicroseconds $ floor $ slotDurF / (2 ** slotsF)

-- | DNS-backed subscription worker. Uses 'subscriber' with a DNS resolver,
-- and 'networkSubscribeTo''. Spawns a thread for each conjunct in the
-- 'DnsDomains' which cycles through alternatives.
dnsSubscriptionWorker
    :: forall pack.
       Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> Word16 -- ^ Default port to use for addresses resolved from DNS domains.
    -> DnsDomains DNS.Domain
    -> MVar (Map NodeId Timer)
    -> IO Millisecond -- ^ Slot duration.
    -> SubscriptionStates NodeId
    -> SendActions
    -> IO ()
dnsSubscriptionWorker logTrace oq defaultPort DnsDomains {..} keepaliveTimerVar slotDuration subStatus sendActions = do
    -- Shared state between threads, used to set the outbound queue known
    -- peers.
    -- Also used to eliminate duplicate subscriptions between threads: you
    -- can give the same DNS domain multiple times, and in case it resolves
    -- to more than one address, more than one will be picked if possible.
    subscriptionsVar <- newMVar Set.empty
    peersVar <- newMVar Map.empty
    traceWith logTrace (Notice, sformat ("dnsSubscriptionWorker: valency "%int) (length dnsDomains))
    forConcurrently_ dnsDomains $ \domain -> do
        subscriber (subscribeToNoDuplicates subscriptionsVar peersVar)
                   (dnsSubscriptionTarget logTrace timeoutError defaultPort domain)
    traceWith logTrace (Notice, sformat ("dnsSubscriptionWorker: all "%int%" threads finished") (length dnsDomains))
  where

    -- Attempt to subscribe to a given address, only if there are no other
    -- threads already subscribed to it. That's possible in case different
    -- DNS names given in alternatives resolve to the same address.
    subscribeToNoDuplicates
      :: MVar (Set NodeId)
      -> MVar (Map (NodeType, NodeId) Int)
      -> SubscribeTo IO NodeId
    subscribeToNoDuplicates subscriptionsVar peersVar nodeId = bracket
        (addToSubscriptionsVar subscriptionsVar nodeId)
        (removeFromSubscriptionsVar subscriptionsVar nodeId)
        (\goAhead -> when goAhead (subscribeTo peersVar nodeId))

    subscribeTo :: MVar (Map (NodeType, NodeId) Int) -> SubscribeTo IO NodeId
    subscribeTo peersVar nodeId = do
        timer <- createTimer nodeId
        duration <- networkSubscribeTo'
            logTrace
            oq
            BucketBehindNatWorker
            NodeRelay
            peersVar
            (keepalive timer)
            subStatus
            sendActions
            nodeId
        deleteTimer nodeId
        timeToWait <- retryInterval duration <$> slotDuration
        threadDelay (fromIntegral timeToWait * 1000)

    -- Gives True if the peer was not in the set and was added.
    -- Gives False if the peer was in the set and therefore was not added.
    addToSubscriptionsVar :: MVar (Set NodeId) -> NodeId -> IO Bool
    addToSubscriptionsVar subscriptionsVar nodeId = modifyMVar subscriptionsVar $ \set ->
        if Set.member nodeId set
        then pure (set, False)
        else let !set' = Set.insert nodeId set in pure (set', True)

    -- To bracket with addToSubscriptionsVar. Do nothing if the Bool is
    -- False, because the peer was not added by this bracket. If True, remove
    -- it from the set.
    removeFromSubscriptionsVar :: MVar (Set NodeId) -> NodeId -> Bool -> IO ()
    removeFromSubscriptionsVar subscriptionsVar nodeId True  = modifyMVar subscriptionsVar $ \set ->
        let !set' = Set.delete nodeId set in pure (set', ())
    removeFromSubscriptionsVar _                _      False = pure ()

    createTimer :: NodeId -> IO Timer
    createTimer nodeId = do
        timer <- newTimer
        modifyMVar_ keepaliveTimerVar $ \timers ->
            let !timers' = Map.insert nodeId timer timers in
            pure timers'
        return timer

    deleteTimer :: NodeId -> IO ()
    deleteTimer nodeId =
        modifyMVar_ keepaliveTimerVar $ \timers ->
            let !timers' = Map.delete nodeId timers in
            pure timers'

    -- When to send a keepalive message.
    keepalive :: Timer -> IO ()
    keepalive keepaliveTimer = do
        time <- slotDuration
        startTimer time keepaliveTimer
        atomically $ waitTimer keepaliveTimer

    -- How long to wait if there's a DNS resolution error.
    -- One slot duration.
    timeoutError :: IO Microsecond
    timeoutError = convertUnit <$> slotDuration
