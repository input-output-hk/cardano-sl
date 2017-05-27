{-# LANGUAGE TypeFamilies #-}

module Pos.DHT.Real.Real
       ( foreverRejoinNetwork
       , K.lookupNode
       , kademliaGetKnownPeers
       , startDHTInstance
       , stopDHTInstance
       ) where

import           Data.Binary               (decode)
import qualified Data.ByteString.Char8     as B8 (unpack)
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import           Formatting                (build, int, sformat, shown, (%))
import           Mockable                  (Async, Catch, Mockable, MonadMockable,
                                            Promise, Throw, catch, catchAll, throw,
                                            waitAnyUnexceptional, withAsync)
import qualified Network.Kademlia          as K
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
import           Pos.DHT.Real.Types        (KademliaDHTInstance (..),
                                            KademliaDHTInstanceConfig (..))
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

-- | Start 'KademliaDHTInstance' with 'KademliaDHTInstanceConfig'.
startDHTInstance
    :: ( MonadIO m
       , Mockable Catch m
       , Mockable Throw m
       , WithLogger m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaDHTInstanceConfig -> m KademliaDHTInstance
startDHTInstance kconf@KademliaDHTInstanceConfig {..} = do
    let host :: String
        host = B8.unpack kdcHost
    logInfo "Generating dht key.."
    kdiKey <- maybe randomDHTKey pure kdcKey
    logInfo $ sformat ("Generated dht key "%build) kdiKey
    shouldRestore <- liftIO $ doesFileExist kdcDumpPath
    kdiHandle <-
        if shouldRestore
        then do logInfo "Restoring DHT Instance from snapshot"
                catchErrors $
                    createKademliaFromSnapshot host kdcPort kademliaConfig =<<
                    decode <$> BS.readFile kdcDumpPath
        else do logInfo "Creating new DHT instance"
                catchErrors $ createKademlia host kdcPort kdiKey kademliaConfig

    logInfo "Created DHT instance"
    let kdiInitialPeers = kdcInitialPeers
    let kdiExplicitInitial = kdcExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    let kdiDumpPath = kdcDumpPath
    pure $ KademliaDHTInstance {..}
  where
    catchErrorsHandler e = do
        logError $ sformat ("Error launching kademlia with options: "%shown%": "%shown) kconf e
        throw e
    catchErrors x = liftIO x `catchAll` catchErrorsHandler

    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText
    createKademlia host port key cfg =
        K.createL host (fromIntegral port) key cfg (log' logDebug) (log' logError)
    createKademliaFromSnapshot host port cfg snapshot =
        K.createLFromSnapshot host (fromIntegral port)
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
    peers <- kademliaGetKnownPeers inst
    logDebug $ sformat ("rejoinNetwork: peers "%build) peers
    when (length peers < neighborsSendThreshold) $ do
      logWarning $ sformat ("Not enough peers: "%int%", threshold is "%int)
                           (length peers) (neighborsSendThreshold :: Int)
      kademliaJoinNetworkNoThrow inst init

withKademliaLogger
    :: ( HasLoggerName m )
    => m a
    -> m a
withKademliaLogger action = modifyLoggerName (<> "kademlia") action

kademliaGetKnownPeers
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
    -> m [DHTNode]
kademliaGetKnownPeers inst = undefined
  --   let myId = kdiKey inst
  --   let initPeers = bool [] (kdiInitialPeers inst) (kdiExplicitInitial inst)
  --   buckets <- liftIO (K.viewBuckets $ kdiHandle inst)
  --   filter ((/= myId) . dhtNodeId) <$> extendPeers myId initPeers buckets
  -- where
  --   extendPeers
  --       :: MonadIO m1
  --       => DHTKey
  --       -> [DHTNode]
  --       -> [[(K.Node DHTKey, Int64)]]
  --       -> m1 [DHTNode]
  --   extendPeers myId initial buckets =
  --       map snd .
  --       HM.toList .
  --       HM.delete myId .
  --       flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial .
  --       HM.fromList . map (\(toDHTNode -> n) -> (dhtNodeId n, n)) <$>
  --       (updateCache $ concatMap getPeersFromBucket buckets)

  --   getPeersFromBucket :: [(K.Node DHTKey, Int64)] -> [K.Node DHTKey]
  --   getPeersFromBucket bucket
  --       | null bucket = []
  --       | otherwise =
  --           let peers = filter ((< enhancedMessageTimeout) . snd) bucket in
  --           map fst $
  --           takeSafe enhancedMessageBroadcast $
  --           bool peers (sortWith snd bucket) (null peers)
  --   takeSafe :: Int -> [a] -> [a]
  --   takeSafe p a
  --       | length a <= p = a
  --       | otherwise = take p a

  --   updateCache :: MonadIO m1 => [K.Node DHTKey] -> m1 [K.Node DHTKey]
  --   updateCache peers =
  --       peers <$ (atomically $ writeTVar (kdiKnownPeersCache inst) peers)

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
       , Mockable Async m
       , Eq (Promise m (Maybe ()))
       , WithLogger m
       , Bi DHTKey
       , Bi DHTData
       )
    => KademliaDHTInstance
    -> [NetworkAddress]
    -> m ()
kademliaJoinNetwork _ [] = throw AllPeersUnavailable
kademliaJoinNetwork inst nodes =
    waitAnyUnexceptional (map (kademliaJoinNetwork' inst) nodes) >>= handleRes
  where
    handleRes (Just _) = pure ()
    handleRes _        = throw AllPeersUnavailable

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
