{-# LANGUAGE TypeFamilies #-}

module Pos.Infra.DHT.Real.Real
       ( kademliaJoinNetwork
       , kademliaJoinNetworkNoThrow
       , kademliaJoinNetworkRetry
       , K.lookupNode
       , kademliaGetKnownPeers
       , startDHTInstance
       , stopDHTInstance
       ) where


-- We'll take 'try' and 'catch' from Control.Exception
-- Universum uses those from Control.Exception.Safe, but in here we never
-- squelch async exceptions so it's ok.
import           Universum hiding (catch, try)

import           Control.Concurrent (threadDelay)
import           Control.Exception (catch, throwIO, try)
import qualified Data.ByteString.Char8 as B8 (unpack)
import qualified Data.ByteString.Lazy as BS
import           Data.List (intersect, (\\))
import           Data.Time.Units (Second, toMicroseconds)
import           Formatting (build, sformat, shown, (%))
import qualified Network.Kademlia as K
import qualified Network.Kademlia.Instance as K (KademliaInstance (state),
                     KademliaState (sTree))
import qualified Network.Kademlia.Tree as K (toView)
import           Serokell.Util (listJson)
import           System.Directory (doesFileExist)

import           Pos.Binary.Class (decodeFull)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.Binary.DHTModel ()
import           Pos.Infra.DHT.Constants (enhancedMessageBroadcast,
                     enhancedMessageTimeout)
import           Pos.Infra.DHT.Model.Types (DHTException (..), DHTKey,
                     DHTNode (..), randomDHTKey)
import           Pos.Infra.DHT.Real.Param (KademliaParams (..))
import           Pos.Infra.DHT.Real.Types (KademliaDHTInstance (..))
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace.Named (TraceNamed, appendName, logError,
                     logInfo, logMessage, logWarning)

kademliaConfig :: K.KademliaConfig
kademliaConfig = K.defaultConfig { K.k = 16 }

-- | Stop chosen 'KademliaDHTInstance'.
stopDHTInstance
    :: MonadIO m
    => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

-- | Start 'KademliaDHTInstance' with 'KademliaParams'.
startDHTInstance
    :: TraceNamed IO
    -> KademliaParams
    -> NetworkAddress -- ^ Default NetworkAddress to bind.
    -> IO KademliaDHTInstance
startDHTInstance logTrace0 kconf@KademliaParams {..} defaultBind = do
    let bindAddr = first B8.unpack (fromMaybe defaultBind kpNetworkAddress)
        extAddr  = maybe bindAddr (first B8.unpack) kpExternalAddress
    logInfo logTrace $ "Generating dht key.."
    kdiKey <- maybe randomDHTKey pure kpKey
    logInfo logTrace $ sformat ("Generated dht key "%build) kdiKey
    kdiDumpPath <- case kpDumpFile of
        Nothing -> pure Nothing
        Just fp -> do
            exists <- doesFileExist fp
            pure $ if exists then Just fp else Nothing
    kdiHandle <- case kdiDumpPath of
        Just dumpFile -> do
            logInfo logTrace "Restoring DHT Instance from snapshot"
            catchErrors $
                createKademliaFromSnapshot bindAddr extAddr kademliaConfig =<<
                (either error identity . decodeFull) <$> BS.readFile dumpFile
        Nothing -> do
            logInfo logTrace "Creating new DHT instance"
            catchErrors $ createKademlia bindAddr extAddr kdiKey kademliaConfig

    logInfo logTrace "Created DHT instance"
    let kdiInitialPeers = kpPeers
    let kdiExplicitInitial = kpExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    pure $ KademliaDHTInstance {..}
  where
    logTrace = appendName "dhtInstance" logTrace0
    catchErrorsHandler :: forall t . SomeException -> IO t
    catchErrorsHandler e = do
        logError logTrace $ sformat ("Error launching kademlia with options: "%shown%": "%shown) kconf e
        throwIO e
    catchErrors x = x `catch` catchErrorsHandler

    log' sev msg = logMessage logTrace sev (toText msg)
    createKademlia bA eA key cfg =
        K.createL bA eA key cfg (log' Log.Debug) (log' Log.Error)
    createKademliaFromSnapshot bA eA cfg snapshot =
        K.createLFromSnapshot bA eA
            cfg snapshot (log' Log.Debug) (log' Log.Error)

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
    :: TraceNamed IO
    -> KademliaDHTInstance
    -> [NetworkAddress]
    -> IO ()
kademliaJoinNetwork _ _ [] = throwIO AllPeersUnavailable
kademliaJoinNetwork logTrace inst (node : nodes) = do
    outcome <- try (kademliaJoinNetwork' logTrace inst node)
    case outcome of
        Left (_e :: DHTException) -> kademliaJoinNetwork logTrace inst nodes
        Right _                   -> return ()

kademliaJoinNetwork'
    :: TraceNamed IO
    -> KademliaDHTInstance
    -> NetworkAddress
    -> IO ()
kademliaJoinNetwork' logTrace inst peer = do
    res <- K.joinNetwork (kdiHandle inst) (toKPeer peer)
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throwIO NodeDown
        K.NodeBanned ->
            logInfo logTrace $
                sformat ("kademliaJoinNetwork: peer " % build % " is banned") peer
        K.IDClash ->
            logInfo logTrace $
            sformat ("kademliaJoinNetwork: peer " % build % " already contains us") peer

-- | Attempt to join a Kademlia network by contacting this list of peers.
--   If none of them are up, a warning is logged but no exception is thrown.
kademliaJoinNetworkNoThrow
    :: TraceNamed IO
    -> KademliaDHTInstance
    -> [NetworkAddress]
    -> IO ()
kademliaJoinNetworkNoThrow logTrace inst peers =
    kademliaJoinNetwork logTrace inst peers `catch` handleJoinE
  where
    handleJoinE AllPeersUnavailable =
        logWarning logTrace $ sformat ("kademliaJoinNetwork: not connected to any of peers "%listJson) peers
    handleJoinE e = throwIO e

-- | Attempt to join a Kademlia network by contacting this list of peers.
--   If none of them are up, retry after a fixed delay.
kademliaJoinNetworkRetry
    :: TraceNamed IO
    -> KademliaDHTInstance
    -> [NetworkAddress]
    -> Second
    -> IO ()
kademliaJoinNetworkRetry logTrace inst peers interval = do
    result <- try $ kademliaJoinNetwork logTrace inst peers
    case result of
      Right _ -> return ()
      Left AllPeersUnavailable -> do
          logWarning logTrace $ sformat ("kademliaJoinNetwork: could not connect to any peers, will retry in "%shown) interval
          threadDelay (fromIntegral (toMicroseconds interval))
          kademliaJoinNetworkRetry logTrace inst peers interval
      Left e -> throwIO e
