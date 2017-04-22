{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.DHT.Real.Real
       ( foreverRejoinNetwork
       , K.lookupNode
       , kademliaGetKnownPeers
       , startDHTInstance
       , stopDHTInstance
       ) where

import           Universum                 hiding (bracket, catchAll, catch)

import           Control.Concurrent.STM    (newTVar, readTVar, writeTVar)
import           Data.Binary               (decode)
import qualified Data.ByteString.Char8     as B8 (unpack)
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import           Data.List                 (intersect, (\\))
import           Formatting                (build, int, sformat, shown, (%))
import           Mockable                  (Async, Catch, Mockable, MonadMockable,
                                            Promise, Throw, catchAll,
                                            throw, waitAnyUnexceptional,
                                            catch, withAsync)
import qualified Network.Kademlia          as K
import           Serokell.Util             (ms, sec)
import           System.Directory          (doesFileExist)
import           System.Wlog               (WithLogger, logDebug, logError, logInfo,
                                            logWarning, usingLoggerName,
                                            HasLoggerName (modifyLoggerName))

import           Pos.Binary.Class          (Bi (..))
import           Pos.Binary.Infra.DHTModel ()
import           Pos.DHT.Constants         (enhancedMessageBroadcast,
                                            neighborsSendThreshold)
import           Pos.DHT.Model.Types       (DHTData, DHTKey, DHTNode (..), randomDHTKey,
                                            DHTException (..))
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
    logDebug $ sformat ("rejoinNetwork: peers " % build) peers
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
kademliaGetKnownPeers inst = do
    let myId = kdiKey inst
    let initialPeers = kdiInitialPeers inst
    let explicitInitial = kdiExplicitInitial inst
    peers <- liftIO $ K.dumpPeers $ kdiHandle inst
    let initPeers = bool [] initialPeers explicitInitial
    filter ((/= myId) . dhtNodeId) <$> extendPeers myId initPeers peers
  where
    extendPeers myId initial peers =
        map snd .
        HM.toList .
        HM.delete myId .
        flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial .
        HM.fromList . map (\(toDHTNode -> n) -> (dhtNodeId n, n)) <$>
        (updateCache =<< selectSufficientNodes myId peers)
    selectSufficientNodes myId l =
        if enhancedMessageBroadcast /= (0 :: Int)
            then concatMapM (getPeersFromBucket enhancedMessageBroadcast)
                     (splitToBuckets myId l)
            else return l
    bucketIndex origin x =
        length . takeWhile (== False) <$> K.distance origin (K.nodeId x)
    insertId origin i hm = do
        bucket <- bucketIndex origin i
        return $ HM.insertWith (++) bucket [i] hm
    splitToBuckets origin peers =
        flip K.usingKademliaInstance (kdiHandle inst) $
        HM.elems <$> foldrM (insertId origin) HM.empty peers
    getPeersFromBucket p bucket = do
        cache <- atomically $ readTVar $ kdiKnownPeersCache inst
        let fromCache = tryTake p $ intersect cache bucket
        return $ fromCache ++ tryTake (p - length fromCache) (bucket \\ cache)
    tryTake x l
        | length l < x = l
        | otherwise = take x l
    updateCache peers = do
        atomically $ writeTVar (kdiKnownPeersCache inst) peers
        return peers

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
    -> [DHTNode]
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
    => KademliaDHTInstance -> DHTNode -> m ()
kademliaJoinNetwork' inst node = do
    let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
    res <- liftIO $ K.joinNetwork (kdiHandle inst) node'
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throw NodeDown
        K.NodeBanned ->
            logInfo $
            sformat ("joinNetwork: node " % build % " is banned") node
        K.IDClash ->
            logInfo $
            sformat ("joinNetwork: node " % build % " already contains us") node

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
    -> [DHTNode]
    -> m ()
kademliaJoinNetworkNoThrow inst peers = kademliaJoinNetwork inst peers `catch` handleJoinE
    where
    handleJoinE AllPeersUnavailable =
        logWarning $ sformat ("Not connected to any of peers " % build) peers
    handleJoinE e = throw e
