{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.Txp.Worker
       ( txpWorkers
       ) where

import           Universum

import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Ssc.Class       (SscWorkersClass)
import           Pos.Util            (mconcatPair)
import           Pos.WorkMode.Class  (WorkMode)

#ifdef WITH_WALLET
import           Data.IP             (IPv4)
import           Data.Time.Units     (Second, Millisecond, convertUnit)
import           Data.Time           (UTCTime, NominalDiffTime, getCurrentTime,
                                      diffUTCTime)
import qualified Data.Map.Strict     as M
import qualified Data.ByteString.Char8 as BS.C8
import           Formatting          (sformat, shown, (%))
import           Mockable            (delay, try)
import qualified Network.DNS         as DNS
import           System.Wlog         (logInfo, logWarning, logError)

import           Pos.Block.Network.Types (MsgSubscribe (..))
import           Pos.Communication   (Conversation (..), ConversationActions (..),
                                      convH, toOutSpecs, worker,
                                      NodeId, withConnectionTo)
import           Pos.Slotting        (getLastKnownSlotDuration)
import           Pos.Util            (lensOf)
import           Pos.Util.TimeWarp   (addressToNodeId)
import           Pos.Launcher.Param  (RelayParams(..))
#endif

-- | All workers specific to transaction processing.
txpWorkers
    :: (SscWorkersClass ssc, WorkMode ssc ctx m)
    => ([WorkerSpec m], OutSpecs)
txpWorkers =
    merge $ []
#if defined(WITH_WALLET)
            ++ [ subscribeWorker ]
#endif
  where
    merge = mconcatPair . map (first pure)

#if defined(WITH_WALLET)

data KnownRelay = Relay {
      -- | When did we find out about this relay?
      relayDiscovered :: UTCTime

      -- | Was this relay reported last call to getPeers?
    , relayActive :: Bool

      -- | When was the last time it _was_ reported by getPeers?
    , relayLastSeen :: UTCTime

      -- | When did we last experience an error communicating with this relay?
    , relayException :: Maybe (UTCTime, SomeException)
    }

type KnownRelays = Map NodeId KnownRelay

-- | We never expect relays to close the connection
data RelayClosedConnection = RelayClosedConnection
  deriving (Show)

instance Exception RelayClosedConnection

subscribeWorker
    :: forall ssc ctx m. (WorkMode ssc ctx m, SscWorkersClass ssc)
    => (WorkerSpec m, OutSpecs)
subscribeWorker = worker subscribeWorkerSpec $ \sendActions -> do
    resolvSeed  <- liftIO $ DNS.makeResolvSeed DNS.defaultResolvConf
    relayParams <- view (lensOf @RelayParams) <$> ask

    let go :: KnownRelays -> m ()
        go relays = do
          slotDur <- getLastKnownSlotDuration
          let delayInterval :: Millisecond
              delayInterval = max (slotDur `div` 4) (convertUnit (5 :: Second))

          now   <- liftIO $ getCurrentTime
          peers <- findRelays relayParams resolvSeed
          let relays' = updateKnownRelays now peers relays

          case preferredRelays now relays' of
            [] -> do
              delay delayInterval
              go relays'
            (relay:_) -> do
              logInfo $ msgConnectingTo relay
              Left ex <- try $ withConnectionTo sendActions relay $ \_peerData ->
                pure $ Conversation $ \conv -> do
                  send conv MsgSubscribe
                  _void :: Maybe Void <- recv conv 0 -- Other side will never send
                  throwM RelayClosedConnection
              logWarning $ msgLostConnection relay
              timeOfEx <- liftIO $ getCurrentTime
              go $ M.adjust (\r -> r { relayException = Just (timeOfEx, ex) })
                            relay
                            relays'

    go M.empty
  where
    -- | Do DNS lookup to find relay nodes
    findRelays :: RelayParams -> DNS.ResolvSeed -> m [NodeId]
    findRelays relayParams@RelayParams{..} resolvSeed =
        go [] rpDNS
      where
        go :: [DNS.DNSError] -> [DNS.Domain] -> m [NodeId]
        go errs [] = do
          logError $ msgDnsFailure errs
          return []
        go errs (dom:doms) = do
          mAddrs <- liftIO $ DNS.withResolver resolvSeed (`DNS.lookupA` dom)
          case mAddrs of
            Left  err   -> go (err:errs) doms
            Right addrs -> return $ map (ipv4ToNodeId relayParams) addrs

    -- | Turn IPv4 address returned by DNS into a NodeId
    ipv4ToNodeId :: RelayParams -> IPv4 -> NodeId
    ipv4ToNodeId RelayParams{..} addr =
        addressToNodeId (BS.C8.pack (show addr), rpPort)

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
    msgConnectingTo   = sformat $ "subscribeWorker: subscribing to " % shown
    msgLostConnection = sformat $ "subscribeWorker: lost connection to " % shown

    msgDnsFailure :: [DNS.DNSError] -> Text
    msgDnsFailure = sformat $ "subscribeWorker: DNS failure: " % shown

subscribeWorkerSpec :: OutSpecs
subscribeWorkerSpec = toOutSpecs [ convH (Proxy @MsgSubscribe) (Proxy @Void) ]

#endif
