{-# LANGUAGE TypeFamilies #-}

module Pos.DHT.Real.Real
       ( kademliaJoinNetwork
       , kademliaJoinNetworkNoThrow
       , kademliaJoinNetworkRetry
       , K.lookupNode
       , kademliaGetKnownPeers
       , startDHTInstance
       , stopDHTInstance
         -- * Exported to avoid compiler warnings
         -- TODO: If we really don't need these functions, remove
       , foreverRejoinNetwork
       , rejoinNetwork
       , withKademliaLogger
       ) where


import           Universum hiding (init)

import           Control.Exception.Safe (try)
import qualified Data.ByteString.Char8 as B8 (unpack)
import qualified Data.ByteString.Lazy as BS
import           Data.List (intersect, (\\))
import           Data.Time.Units (Second, fromMicroseconds)
import           Formatting (build, int, sformat, shown, (%))
import           Mockable (Delay, Mockable, MonadMockable, delay, withAsync)
import qualified Network.Kademlia as K
import qualified Network.Kademlia.Instance as K (KademliaInstance (state), KademliaState (sTree))
import qualified Network.Kademlia.Tree as K (toView)
import           Serokell.Util (listJson)
import           System.Directory (doesFileExist)
import           System.Wlog (HasLoggerName (modifyLoggerName), WithLogger, logDebug, logError,
                              logInfo, logWarning, usingLoggerName)

import           Pos.Binary.Class (Bi (..), decodeFull)
import           Pos.Binary.Infra.DHTModel ()
import           Pos.DHT.Constants (enhancedMessageBroadcast, enhancedMessageTimeout,
                                    neighborsSendThreshold)
import           Pos.DHT.Model.Types (DHTData, DHTException (..), DHTKey, DHTNode (..),
                                      randomDHTKey)
import           Pos.DHT.Real.Param (KademliaParams (..))
import           Pos.DHT.Real.Types (KademliaDHTInstance (..))
import           Pos.Util.LogSafe (logInfoS)
import           Pos.Util.TimeLimit (runWithRandomIntervals)
import           Pos.Util.TimeWarp (NetworkAddress)

kademliaConfig :: K.KademliaConfig
kademliaConfig = K.defaultConfig { K.k = 16 }

-- | Rejoin the Kademlia network at random intervals (in a separate thread)
--   while some action is going. The rejoining thread will be killed when
--   the action is finished.
foreverRejoinNetwork
    :: ( MonadMockable m
       , MonadIO m
       , WithLogger m
       )
    => KademliaDHTInstance
    -> m a
    -> m a
foreverRejoinNetwork inst action = withAsync
    (runWithRandomIntervals (fromMicroseconds 500000) (fromMicroseconds 5000000) (rejoinNetwork inst))
    (const action)

-- | Stop chosen 'KademliaDHTInstance'.
stopDHTInstance
    :: MonadIO m
    => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

-- | Start 'KademliaDHTInstance' with 'KademliaParams'.
startDHTInstance
    :: ( MonadIO m
       , MonadCatch m
       , WithLogger m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaParams
    -> NetworkAddress -- ^ Default NetworkAddress to bind.
    -> m KademliaDHTInstance
startDHTInstance kconf@KademliaParams {..} defaultBind = do
    let bindAddr = first B8.unpack (fromMaybe defaultBind kpNetworkAddress)
        extAddr  = maybe bindAddr (first B8.unpack) kpExternalAddress
    logInfo "Generating dht key.."
    kdiKey <- maybe randomDHTKey pure kpKey
    logInfoS $ sformat ("Generated dht key "%build) kdiKey
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
                (either error identity . decodeFull) <$> BS.readFile dumpFile
        Nothing -> do
            logInfo "Creating new DHT instance"
            catchErrors $ createKademlia bindAddr extAddr kdiKey kademliaConfig

    logInfo "Created DHT instance"
    let kdiInitialPeers = kpPeers
    let kdiExplicitInitial = kpExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    pure $ KademliaDHTInstance {..}
  where
    catchErrorsHandler e = do
        logError $ sformat ("Error launching kademlia with options: "%shown%": "%shown) kconf e
        throwM e
    catchErrors x = liftIO x `catchAny` catchErrorsHandler

    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText
    createKademlia bA eA key cfg =
        K.createL bA eA key cfg (log' logDebug) (log' logError)
    createKademliaFromSnapshot bA eA cfg snapshot =
        K.createLFromSnapshot bA eA
            cfg snapshot (log' logDebug) (log' logError)

rejoinNetwork
    :: ( MonadIO m
       , MonadCatch m
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

-- | Attempt to join a Kademlia network by contacting this list of peers.
--   If none of them are up, throw 'AllPeersUnavailable'.
kademliaJoinNetwork
    :: ( MonadIO m
       , MonadCatch m
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance
    -> [NetworkAddress]
    -> m ()
kademliaJoinNetwork _ [] = throwM AllPeersUnavailable
kademliaJoinNetwork inst (node : nodes) = do
    outcome <- try (kademliaJoinNetwork' inst node)
    case outcome of
        Left (_e :: DHTException) -> kademliaJoinNetwork inst nodes
        Right _                   -> return ()

kademliaJoinNetwork'
    :: ( MonadIO m
       , MonadThrow m
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance -> NetworkAddress -> m ()
kademliaJoinNetwork' inst peer = do
    res <- liftIO $ K.joinNetwork (kdiHandle inst) (toKPeer peer)
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throwM NodeDown
        K.NodeBanned ->
            logInfo $
            sformat ("kademliaJoinNetwork: peer " % build % " is banned") peer
        K.IDClash ->
            logInfo $
            sformat ("kademliaJoinNetwork: peer " % build % " already contains us") peer

-- | Attempt to join a Kademlia network by contacting this list of peers.
--   If none of them are up, a warning is logged but no exception is thrown.
kademliaJoinNetworkNoThrow
    :: ( MonadIO m
       , MonadCatch m
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
        logWarning $ sformat ("kademliaJoinNetwork: not connected to any of peers "%listJson) peers
    handleJoinE e = throwM e

-- | Attempt to join a Kademlia network by contacting this list of peers.
--   If none of them are up, retry after a fixed delay.
kademliaJoinNetworkRetry
    :: ( MonadIO m
       , MonadCatch m
       , Mockable Delay m
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance
    -> [NetworkAddress]
    -> Second
    -> m ()
kademliaJoinNetworkRetry inst peers interval = do
    result <- try $ kademliaJoinNetwork inst peers
    case result of
      Right _ -> return ()
      Left AllPeersUnavailable -> do
          logWarning $ sformat ("kademliaJoinNetwork: could not connect to any peers, will retry in "%shown) interval
          delay interval
          kademliaJoinNetworkRetry inst peers interval
      Left e -> throwM e
