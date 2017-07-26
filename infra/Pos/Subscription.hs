{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Subscription
    ( SubscriptionMode
    , subscriptionWorkers
    ) where

import           Universum

import           Data.Time.Units         (Second, Millisecond, convertUnit)
import           Data.Time               (UTCTime, NominalDiffTime, getCurrentTime,
                                          diffUTCTime)
import           Formatting              (sformat, shown, (%))
import           Network.Broadcast.OutboundQueue.Types (peersFromList)
import           System.Wlog             (WithLogger, logInfo, logWarning, logError)
import qualified Data.Map.Strict         as M
import           Mockable                (Mockable, Throw, Delay, Catch, delay, try, throw)
import qualified Network.DNS             as DNS
import           Node.Message.Class      (Message)

import           Pos.Binary.Class           (Bi)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec, MsgSubscribe (..),
                                             Conversation (..), ConversationActions (..),
                                             convH, toOutSpecs, worker,
                                             NodeId, withConnectionTo, SendActions)
import           Pos.KnownPeers             (MonadKnownPeers(..))
import           Pos.Network.Types          (NetworkConfig(..), DnsDomains(..),
                                             resolveDnsDomains, NodeType (..))

type SubscriptionMode m =
    ( MonadIO m
    , WithLogger m
    , Mockable Throw m
    , Mockable Catch m
    , Mockable Delay m
    , MonadKnownPeers m
    , Message MsgSubscribe
    , Bi MsgSubscribe
    , Message Void
    )

data KnownRelay = Relay {
      -- | When did we find out about this relay?
      relayDiscovered :: UTCTime

      -- | Was this relay reported last call to findRelays?
    , relayActive :: Bool

      -- | When was the last time it _was_ reported by findRelays?
    , relayLastSeen :: UTCTime

      -- | When did we last experience an error communicating with this relay?
    , relayException :: Maybe (UTCTime, SomeException)
    }

type KnownRelays = Map NodeId KnownRelay

activeRelays :: KnownRelays -> [NodeId]
activeRelays = map fst . filter (relayActive . snd) . M.toList

-- | We never expect relays to close the connection
data RelayClosedConnection = RelayClosedConnection
  deriving (Show)

instance Exception RelayClosedConnection

subscriptionWorkers
    :: forall m. (SubscriptionMode m)
    => NetworkConfig -> DnsDomains -> ([WorkerSpec m], OutSpecs)
subscriptionWorkers networkCfg dnsDomains = first (:[]) <$>
    worker subscriptionWorkerSpec $ subscriptionWorker' networkCfg dnsDomains
  where
    subscriptionWorkerSpec :: OutSpecs
    subscriptionWorkerSpec = toOutSpecs [ convH (Proxy @MsgSubscribe) (Proxy @Void) ]

subscriptionWorker'
    :: forall m. (SubscriptionMode m)
    => NetworkConfig -> DnsDomains -> SendActions m -> m ()
subscriptionWorker' networkCfg dnsDomains sendActions =
    loop M.empty
  where
    loop :: KnownRelays -> m ()
    loop oldRelays = do
      slotDur <- error "oops" -- getLastKnownSlotDuration
      now     <- liftIO $ getCurrentTime
      peers   <- findRelays

      let delayInterval :: Millisecond
          delayInterval = max (slotDur `div` 4) (convertUnit (5 :: Second))

          updatedRelays :: KnownRelays
          updatedRelays = updateKnownRelays now peers oldRelays

      -- Declare all active relays as a single list of alternative relays
      updateKnownPeers $ \peersOld -> do
        -- NOTE: The assumption is that a single part of the code is
        -- responsible for maintaining the set of known peers. For behind NAT
        -- nodes, this is the responsibility of the subscription worker; for
        -- P2P/transitional nodes, this is the responsibility of the
        -- Kademlia worker; and for statically known sets of peers (core nodes
        -- and light wallets) this happens at queue initialization time.
        -- Here we just check that nobody else interferred with the set of
        -- peers, just as a sanity check.
        let expectedOld = peersFromList [(NodeRelay, activeRelays oldRelays)]
        if peersOld == expectedOld
          then peersFromList [(NodeRelay, activeRelays updatedRelays)]
          else error "Invariant violated in subscriptionWorker"

      -- Subscribe only to a single relay (if we found one)
      case preferredRelays now updatedRelays of
        [] -> do
          logError msgNoRelays
          delay delayInterval
          loop updatedRelays
        (relay:_) -> do
          logInfo $ msgConnectingTo relay
          Left ex <- try $ withConnectionTo sendActions relay $ \_peerData ->
            pure $ Conversation $ \conv -> do
              send conv MsgSubscribe
              _void :: Maybe Void <- recv conv 0 -- Other side will never send
              throw RelayClosedConnection
          logWarning $ msgLostConnection relay
          timeOfEx <- liftIO $ getCurrentTime
          loop $ M.adjust (\r -> r { relayException = Just (timeOfEx, ex) })
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
        . sortBy (comparing (relayDiscovered . snd))
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

    msgConnectingTo, msgLostConnection :: NodeId -> Text
    msgConnectingTo   = sformat $ "subscriptionWorker: subscribing to " % shown
    msgLostConnection = sformat $ "subscriptionWorker: lost connection to " % shown

    msgDnsFailure :: [DNS.DNSError] -> Text
    msgDnsFailure = sformat $ "subscriptionWorker: DNS failure: " % shown

    msgNoRelays :: Text
    msgNoRelays = sformat $ "subscriptionWorker: no relays found"
