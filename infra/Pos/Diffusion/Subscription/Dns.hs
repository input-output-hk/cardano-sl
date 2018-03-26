-- | 'SubscriptionTarget' backed by DNS.

module Pos.Diffusion.Subscription.Dns
    ( dnsSubscriptionTarget
    , dnsSubscriptionWorker
    , retryInterval
    ) where

import           Universum hiding (atomically)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (forConcurrently_)
import           Control.Concurrent.STM (atomically)
import           Control.Exception (SomeException, IOException, toException)
import           Data.Either (partitionEithers)
import qualified Data.Map.Strict as Map
import           Data.Time.Units (Microsecond, Millisecond, toMicroseconds,
                                  fromMicroseconds)
import           Formatting (sformat, shown, int, (%))
import qualified Network.Broadcast.OutboundQueue as OQ
import qualified Network.DNS as DNS

import           Pos.Communication.Protocol (SendActions)
import           Pos.Diffusion.Subscription.Common (SubscriptionMessageConstraints,
                                                    networkSubscribeTo')
import           Pos.Diffusion.Subscription.Subscriber (SubscriptionTarget (..),
                                                        subscriber)
import           Pos.Diffusion.Types (SubscriptionStatus (..))
import           Pos.Network.DnsDomains (NodeAddr)
import           Pos.Network.Types (Bucket (..), DnsDomains (..), NodeId (..),
                                    NodeType (..), resolveDnsDomains)
import           Pos.Util.Timer (Timer, waitTimer)
import           Pos.Util.Trace (Trace, Severity (..), traceWith)

-- | Resolve a fixed list of names to a NodeId (using a given port) repeatedly.
-- If the resolution may give more than one address, the addresses are given
-- in-order, and a new resolution will take place once they have all been
-- consumed.
dnsSubscriptionTarget
    :: Trace IO (Severity, Text)
    -> IO Microsecond -- ^ How long to wait between resolutions (after all
                      -- addresses from a single resolution have been tried).
    -> Word16
    -> [NodeAddr DNS.Domain]
    -> SubscriptionTarget IO NodeId
dnsSubscriptionTarget logTrace timeoutExhausted defaultPort addrs = SubscriptionTarget $ do
    nodeIds <- resolve
    listTargets nodeIds

  where

    listTargets :: [NodeId] -> IO (NodeId, SubscriptionTarget IO NodeId)
    -- After exhausting all of them, wait a while.
    listTargets [] = do
        timeoutExhausted >>= threadDelay . fromIntegral
        getSubscriptionTarget (dnsSubscriptionTarget logTrace timeoutExhausted defaultPort addrs)
    listTargets (nodeId : nodeIds) = pure (nodeId, SubscriptionTarget (listTargets nodeIds))

    resolve :: IO [NodeId]
    resolve = do
        outcome <- fmap Right (resolveOne logTrace defaultPort addrs)
            `catch` handleDnsError
            `catch` handleIoError
        case outcome of
            Left err -> do
                traceWith logTrace (Error, formatErr err)
                -- TODO wait a while?
                resolve
            Right nodeIds -> pure nodeIds

    handleDnsError :: DNS.DNSError -> IO (Either SomeException t)
    handleDnsError = return . Left . toException

    handleIoError :: IOException -> IO (Either SomeException t)
    handleIoError = return . Left . toException

    formatErr :: SomeException -> Text
    formatErr err = sformat ("exception while resolving domains "%shown%": "%shown) addrs err

-- Find peers via DNS, preserving order.
-- In case multiple addresses are returned for one name, they're flattened
-- and we forget the boundaries, but all of the addresses for a given name
-- are adjacent.
resolveOne :: Trace IO (Severity, Text) -> Word16 -> [NodeAddr DNS.Domain] -> IO [NodeId]
resolveOne logTrace defaultPort nodeAddrs = do
    mNodeIds <- resolveDnsDomains defaultPort nodeAddrs
    let (errs, nids_) = partitionEithers mNodeIds
        nids = mconcat nids_
    when (null nids)       $ traceWith logTrace (Error, msgNoRelays)
    when (not (null errs)) $ traceWith logTrace (Error, msgDnsFailure errs)
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
-- and 'networkSubscribeTo''. Runs a thread for each conjunct in the
-- 'DnsDomains' which cycles through alternatives.
dnsSubscriptionWorker
    :: forall pack .
       ( SubscriptionMessageConstraints )
    => Trace IO (Severity, Text)
    -> OQ.OutboundQ pack NodeId Bucket
    -> Word16 -- ^ Default port to use for addresses resolved from DNS domains.
    -> DnsDomains DNS.Domain
    -> Timer
    -> IO Millisecond -- ^ Last slot duration.
    -> TVar (Map NodeId SubscriptionStatus)
    -> SendActions
    -> IO ()
dnsSubscriptionWorker logTrace oq defaultPort DnsDomains {..} keepaliveTimer nextSlotDuration status sendActions = do
    peersVar <- newMVar Map.empty
    forConcurrently_ dnsDomains $ \domain -> do
        duration <- newMVar 0
        subscriber (networkSubscribeTo' logTrace oq BucketBehindNatWorker NodeRelay peersVar keepalive status duration sendActions)
                   (dnsSubscriptionTarget logTrace (timeoutExhausted duration) defaultPort domain)
  where
    keepalive _ = atomically $ waitTimer keepaliveTimer
    timeoutExhausted :: MVar Millisecond -> IO Microsecond
    timeoutExhausted duration = do
        d <- retryInterval <$> swapMVar duration 0 <*> nextSlotDuration
        traceWith logTrace (Notice, sformat ("dnsSubscriptionWorker: waiting "%int%"ms before trying again")
            (toMicroseconds d `div` 1000))
        pure (fromIntegral d * 1000)
