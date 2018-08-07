-- | 'SubscriptionTarget' backed by DNS.

module Pos.Infra.Diffusion.Subscription.Dns
    ( dnsSubscriptionTarget
    , dnsSubscriptionWorker
    , retryInterval
    ) where

import           Universum hiding (atomically)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM (atomically)
import           Control.Exception (IOException, SomeException, toException)
import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
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
import           Pos.Util.Timer (Timer, startTimer, waitTimer)
import           Pos.Util.Trace (Severity (..), Trace, traceWith)

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

    listTargets :: [NodeId] -> IO (Maybe (NodeId, SubscriptionTarget IO NodeId))
    listTargets [] = getSubscriptionTarget (dnsSubscriptionTarget logTrace timeoutError defaultPort addrs)
    listTargets (nodeId : nodeIds) = pure (Just (nodeId, SubscriptionTarget (listTargets nodeIds)))

    resolve :: IO [NodeId]
    resolve = do
        outcome <- fmap Right (resolveOne logTrace defaultPort addrs)
            `catch` handleDnsError
            `catch` handleIoError
        case outcome of
            Left err -> do
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
-- In case multiple addresses are returned for one name, they're flattened
-- and we forget the boundaries, but all of the addresses for a given name
-- are adjacent.
resolveOne
    :: Trace IO (Severity, Text)
    -> Word16
    -> [NodeAddr DNS.Domain]
    -> IO [NodeId]
resolveOne logTrace defaultPort nodeAddrs = do
    mNodeIds <- resolveDnsDomains defaultPort nodeAddrs
    let (errs, nids_) = partitionEithers mNodeIds
        nids = mconcat nids_
    when (null nids)   $ traceWith logTrace (Error, msgNoRelays)
    unless (null errs) $ traceWith logTrace (Error, msgDnsFailure errs)
    return nids

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
    -> Timer
    -> IO Millisecond -- ^ Slot duration.
    -> SubscriptionStates NodeId
    -> SendActions
    -> IO ()
dnsSubscriptionWorker logTrace oq defaultPort DnsDomains {..} keepaliveTimer slotDuration subStatus sendActions = do
    -- Shared state between threads, used to set the outbound queue known
    -- peers.
    peersVar <- newMVar Map.empty
    traceWith logTrace (Notice, sformat ("dnsSubscriptionWorker: valency "%int) (length dnsDomains))
    forConcurrently_ dnsDomains $ \domain -> do
        subscriber (subscribeTo peersVar)
                   (dnsSubscriptionTarget logTrace timeoutError defaultPort domain)
    traceWith logTrace (Notice, sformat ("dnsSubscriptionWorker: all "%int%" threads finished") (length dnsDomains))
  where

    subscribeTo :: MVar (Map (NodeType, NodeId) Int) -> SubscribeTo IO NodeId
    subscribeTo peersVar nodeId = do
        duration <- networkSubscribeTo'
            logTrace
            oq
            BucketBehindNatWorker
            NodeRelay
            peersVar
            keepalive
            subStatus
            sendActions
            nodeId
        timeToWait <- retryInterval duration <$> slotDuration
        threadDelay (fromIntegral timeToWait * 1000)

    -- When to send a keepalive message.
    keepalive :: IO ()
    keepalive = do
        time <- slotDuration
        startTimer time keepaliveTimer
        atomically $ waitTimer keepaliveTimer

    -- How long to wait if there's a DNS resolution error.
    -- One slot duration.
    timeoutError :: IO Microsecond
    timeoutError = convertUnit <$> slotDuration
