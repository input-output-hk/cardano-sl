{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Subscription.Dns
    ( dnsSubscriptionWorker
    ) where

import qualified Data.Map.Strict                       as M
import           Data.Time                             (NominalDiffTime, UTCTime,
                                                        diffUTCTime, getCurrentTime)
import           Data.Time.Units                       (Millisecond, Second, convertUnit)
import           Formatting                            (sformat, shown, (%))
import qualified Network.DNS                           as DNS
import           System.Wlog                           (logError)
import           Universum

import           Mockable                              (Delay, Mockable, delay)
import           Network.Broadcast.OutboundQueue.Types (peersFromList)

import           Pos.Communication.Protocol            (Worker)
import           Pos.KnownPeers                        (MonadKnownPeers (..))
import           Pos.Network.Types                     (DnsDomains (..), Bucket(..),
                                                        NetworkConfig (..), NodeId (..),
                                                        NodeType (..), resolveDnsDomains)
import           Pos.Slotting                          (MonadSlotsData,
                                                        getLastKnownSlotDuration)
import           Pos.Subscription.Common

data KnownRelay = Relay {
      -- | When did we find out about this relay?
      relayDiscovered :: UTCTime

      -- | Was this relay reported last call to findRelays?
    , relayActive     :: Bool

      -- | When was the last time it _was_ reported by findRelays?
    , relayLastSeen   :: UTCTime

      -- | When did we last experience an error communicating with this relay?
    , relayException  :: Maybe (UTCTime, SubscriptionTerminationReason)
    }

type KnownRelays = Map NodeId KnownRelay

activeRelays :: KnownRelays -> [NodeId]
activeRelays = map fst . filter (relayActive . snd) . M.toList

dnsSubscriptionWorker
    :: forall kademlia m. (SubscriptionMode m, Mockable Delay m, MonadSlotsData m)
    => NetworkConfig kademlia -> DnsDomains -> Worker m
dnsSubscriptionWorker networkCfg dnsDomains sendActions =
    loop M.empty
  where
    loop :: KnownRelays -> m ()
    loop oldRelays = do
      slotDur <- getLastKnownSlotDuration
      now     <- liftIO $ getCurrentTime
      peers   <- findRelays

      let delayInterval :: Millisecond
          delayInterval = max (slotDur `div` 4) (convertUnit (5 :: Second))

          updatedRelays :: KnownRelays
          updatedRelays = updateKnownRelays now peers oldRelays

      -- Declare all active relays as a single list of alternative relays
      updatePeersBucket BucketBehindNatWorker $ \_ ->
        peersFromList [(NodeRelay, activeRelays updatedRelays)]

      -- Subscribe only to a single relay (if we found one)
      --
      -- TODO: Make it configurable how many relays we subscribe to (should
      -- probably share logic with the Kademlia worker).
      case preferredRelays now updatedRelays of
        [] -> do
          logError msgNoRelays
          delay delayInterval
          loop updatedRelays
        (relay:_) -> do
          terminationReason <- subscribeTo sendActions relay
          timeOfEx <- liftIO $ getCurrentTime
          loop $ M.adjust (\r -> r { relayException = Just (timeOfEx, terminationReason) })
                          relay
                          updatedRelays

    -- Find relays
    findRelays :: m [NodeId]
    findRelays = do
        mNodeIds <- liftIO $ resolveDnsDomains networkCfg dnsDomains
        case mNodeIds of
          Left errs -> logError (msgDnsFailure errs) >> return []
          Right ids -> return ids

    -- Suitable relays in order of preference
    --
    -- We prefer older relays over newer ones
    preferredRelays :: UTCTime -> KnownRelays -> [NodeId]
    preferredRelays now =
          map fst
        . sortOn (relayDiscovered . snd)
        . filter (relaySuitable now . snd)
        . M.toList

    -- Suitable relay (one that we might try to connect to)
    relaySuitable :: UTCTime -> KnownRelay -> Bool
    relaySuitable now Relay{..} = and [
          relayActive
        , case relayException of
            Nothing -> True
            Just (timeOfErr, _err) ->
              now `diffUTCTime` timeOfErr > errorExpiry
        ]

    -- Time after an error after which we reconsider a relay (in sec.)
    errorExpiry :: NominalDiffTime
    errorExpiry = 60

    updateKnownRelays :: UTCTime -> [NodeId] -> KnownRelays -> KnownRelays
    updateKnownRelays now =
        M.mergeWithKey
          -- Relays we already knew about
          (\_nodeId () relay -> Just $ relay { relayLastSeen = now
                                             , relayActive   = True
                                             })
          -- Newly discovered delays
          (M.map $ \() -> initKnownRelay now)
          -- Relays that seem to have disappeared
          (M.map $ \relay -> relay { relayActive = False })
      . M.fromList
      . map (, ())

    initKnownRelay :: UTCTime -> KnownRelay
    initKnownRelay now = Relay {
          relayDiscovered = now
        , relayActive     = True
        , relayLastSeen   = now
        , relayException  = Nothing
        }

    msgDnsFailure :: [DNS.DNSError] -> Text
    msgDnsFailure = sformat $ "subscriptionWorker: DNS failure: " % shown

    msgNoRelays :: Text
    msgNoRelays = sformat $ "subscriptionWorker: no relays found"
