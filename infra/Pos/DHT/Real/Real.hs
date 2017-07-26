{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Pos.DHT.Real.Real
       ( kademliaJoinNetwork
       , K.lookupNode
       , kademliaGetKnownPeers
       , startDHTInstance
       , stopDHTInstance
       ) where

import           Control.Concurrent.STM    (retry)
import qualified Data.ByteString.Char8     as B8 (unpack)
import qualified Data.ByteString.Lazy      as BS
import           Data.List                 (intersect, (\\))
import qualified Data.Store                as Store
import           Formatting                (build, int, sformat, shown, (%))
import           Mockable                  (Async, Catch, Mockable, MonadMockable,
                                            Promise, Throw, catch, catchAll, throw,
                                            waitAnyUnexceptional, withAsync, try)
import qualified Network.Kademlia          as K
import qualified Network.Kademlia.Tree     as K (toView)
import qualified Network.Kademlia.Instance as K (KademliaState (sTree), KademliaInstance (state))
import           Serokell.Util             (listJson, ms, sec)
import           System.Directory          (doesFileExist)
import           System.Wlog               (HasLoggerName (modifyLoggerName), WithLogger,
                                            logDebug, logError, logInfo, logWarning,
                                            usingLoggerName)
import           Universum                 hiding (bracket, catch, catchAll)

import           Pos.Binary.Class          (Bi (..))
import           Pos.Binary.Infra.DHTModel ()
import           Pos.DHT.Constants         (enhancedMessageBroadcast,
                                            enhancedMessageTimeout,
                                            neighborsSendThreshold)
import           Pos.DHT.Model.Types       (DHTData, DHTException (..), DHTKey,
                                            DHTNode (..), randomDHTKey)
import           Pos.DHT.Real.Param        (KademliaParams (..))
import           Pos.DHT.Real.Types        (KademliaDHTInstance (..))
import           Pos.Network.Types         (NodeType)
import           Pos.Util.TimeLimit        (runWithRandomIntervals')
import           Pos.Util.TimeWarp         (NetworkAddress)

kademliaConfig :: K.KademliaConfig
kademliaConfig = K.defaultConfig { K.k = 16 }

-- | Rejoin the Kademlia network at random intervals (in a separate thread)
--   while some action is going. The rejoining thread will be killed when
--   the action is finished.
foreverRejoinNetwork
    :: ( MonadMockable m
       , Eq (Promise m (Maybe ()))
       , MonadIO m
       , WithLogger m
       )
    => KademliaDHTInstance
    -> m a
    -> m a
foreverRejoinNetwork inst action = withAsync
    (runWithRandomIntervals' (ms 500) (sec 5) (rejoinNetwork inst))
    (const action)

-- | Stop chosen 'KademliaDHTInstance'.
stopDHTInstance
    :: MonadIO m
    => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

-- | Start 'KademliaDHTInstance' with 'KademliaParams'.
startDHTInstance
    :: ( MonadIO m
       , Mockable Catch m
       , Mockable Throw m
       , WithLogger m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaParams
    -> NodeType -- ^ Type to assign to peers discovered via Kademlia
    -> Bool     -- ^ True if Kademlia peers should populate known peers (MonadKnownPeers)
    -> m KademliaDHTInstance
startDHTInstance kconf@KademliaParams {..} peerType subscribe = do
    let bindAddr = first B8.unpack kpNetworkAddress
        extAddr  = maybe bindAddr (first B8.unpack) kpExternalAddress
    logInfo "Generating dht key.."
    kdiKey <- maybe randomDHTKey pure kpKey
    logInfo $ sformat ("Generated dht key "%build) kdiKey
    kdiDumpPath <- case kpDumpFile of
        Nothing -> pure Nothing
        Just fp -> do
            exists <- liftIO (doesFileExist fp)
            pure $ if exists then Just fp else Nothing
    kdiHandle <- case kdiDumpPath of
        Just dumpFile -> do
            logInfo "Restoring DHT Instance from snapshot"
            catchErrors $
                createKademliaFromSnapshot bindAddr extAddr kademliaConfig =<<
                (Store.decodeEx . BS.toStrict) <$> BS.readFile dumpFile
        Nothing -> do
            logInfo "Creating new DHT instance"
            catchErrors $ createKademlia bindAddr extAddr kdiKey kademliaConfig

    logInfo "Created DHT instance"
    let kdiInitialPeers = kpPeers
    let kdiExplicitInitial = kpExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    let kdiPeerType = peerType
        kdiSubscribe = subscribe
        kdiValency = kpValency
        kdiFallbacks = kpFallbacks
    pure $ KademliaDHTInstance {..}
  where
    catchErrorsHandler e = do
        logError $ sformat ("Error launching kademlia with options: "%shown%": "%shown) kconf e
        throw e
    catchErrors x = liftIO x `catchAll` catchErrorsHandler

    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText
    createKademlia bA eA key cfg =
        K.createL bA eA key cfg (log' logDebug) (log' logError)
    createKademliaFromSnapshot bA eA cfg snapshot =
        K.createLFromSnapshot bA eA
            cfg snapshot (log' logDebug) (log' logError)

rejoinNetwork
    :: ( MonadIO m
       , Mockable Async m
       , Mockable Catch m
       , Mockable Throw m
       , Eq (Promise m (Maybe ()))
       , WithLogger m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaDHTInstance
    -> m ()
rejoinNetwork inst = withKademliaLogger $ do
    let init = kdiInitialPeers inst
    peers <- atomically $ kademliaGetKnownPeers inst
    logDebug $ sformat ("rejoinNetwork: peers "%listJson) peers
    when (length peers < neighborsSendThreshold) $ do
        logWarning $ sformat ("Not enough peers: "%int%", threshold is "%int)
                             (length peers) (neighborsSendThreshold :: Int)
        kademliaJoinNetworkNoThrow inst init

withKademliaLogger
    :: ( HasLoggerName m )
    => m a
    -> m a
withKademliaLogger action = modifyLoggerName (<> "kademlia") action

-- | Return a list of known peers.
--
-- You can get DHTNode using @toDHTNode@ and Kademlia function @peersToNodeIds@.
kademliaGetKnownPeers
    :: KademliaDHTInstance
    -> STM [NetworkAddress]
kademliaGetKnownPeers inst = do
    let kInst = kdiHandle inst
        treeVar = K.sTree (K.state kInst)
    let initNetAddrs = bool [] (kdiInitialPeers inst) (kdiExplicitInitial inst)
    buckets <- fmap K.toView (readTVar treeVar)
    extendPeers (kdiKey inst) initNetAddrs buckets
  where
    extendPeers
        :: DHTKey
        -> [NetworkAddress]
        -> [[(K.Node DHTKey, Int64)]]
        -> STM [NetworkAddress]
    extendPeers myKey initial buckets = do
        cache <- readTVar $ kdiKnownPeersCache inst
        fromBuckets <- updateCache $ concatMap (getPeersFromBucket cache myKey) buckets
         -- Concat with initial peers and select unique.
        pure $ ordNub $ fromBuckets ++ initial

    getPeersFromBucket :: [NetworkAddress] -> DHTKey -> [(K.Node DHTKey, Int64)] -> [NetworkAddress]
    getPeersFromBucket cache myKey bucket
        | null bucket = []
        | otherwise = do
            let toNetAddr = dhtAddr . toDHTNode . fst
            let notMe x = K.nodeId (fst x) /= myKey
            let latestNodes = filter (\x-> snd x < enhancedMessageTimeout && notMe x) bucket
            if null latestNodes then
                map toNetAddr $ filter notMe $ sortWith snd bucket
            else do
                let latestPeers = map toNetAddr latestNodes
                let fromCache = takeSafe enhancedMessageBroadcast (cache `intersect` latestPeers)
                fromCache ++ takeSafe (enhancedMessageBroadcast - length fromCache) (latestPeers \\ cache)

    takeSafe :: Int -> [a] -> [a]
    takeSafe p a
        | length a <= p = a
        | otherwise = take p a

    updateCache :: [NetworkAddress] -> STM [NetworkAddress]
    updateCache peers =
        peers <$ (writeTVar (kdiKnownPeersCache inst) peers)

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer K.Peer{..} = (encodeUtf8 peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (decodeUtf8 peerHost) (fromIntegral peerPort)

kademliaJoinNetwork
    :: ( MonadIO m
       , Mockable Catch m
       , Mockable Throw m
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance
    -> [NetworkAddress]
    -> m ()
kademliaJoinNetwork _ [] = throw AllPeersUnavailable
kademliaJoinNetwork inst (node : nodes) = do
    outcome <- try (kademliaJoinNetwork' inst node)
    case outcome of
        Left (e :: DHTException) -> kademliaJoinNetwork inst nodes
        Right _ -> return ()

kademliaJoinNetwork'
    :: ( MonadIO m
       , Mockable Throw m
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance -> NetworkAddress -> m ()
kademliaJoinNetwork' inst peer = do
    res <- liftIO $ K.joinNetwork (kdiHandle inst) (toKPeer peer)
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throw NodeDown
        K.NodeBanned ->
            logInfo $
            sformat ("joinNetwork: peer " % build % " is banned") peer
        K.IDClash ->
            logInfo $
            sformat ("joinNetwork: peer " % build % " already contains us") peer

kademliaJoinNetworkNoThrow
    :: ( MonadIO m
       , Mockable Catch m
       , Mockable Throw m
       , Mockable Async m
       , Eq (Promise m (Maybe ()))
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance
    -> [NetworkAddress]
    -> m ()
kademliaJoinNetworkNoThrow inst peers = kademliaJoinNetwork inst peers `catch` handleJoinE
    where
    handleJoinE AllPeersUnavailable =
        logWarning $ sformat ("Not connected to any of peers "%listJson) peers
    handleJoinE e = throw e
