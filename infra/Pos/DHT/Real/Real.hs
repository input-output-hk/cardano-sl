{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.DHT.Real.Real
       ( runKademliaDHT
       , startDHTInstance
       , stopDHTInstance
       ) where

import           Universum                 hiding (bracket, catchAll)

import           Control.Concurrent.STM    (newTVar, writeTVar)
import           Data.Binary               (decode)
import qualified Data.ByteString.Lazy      as BS
import qualified Data.HashMap.Strict       as HM
import           Data.List                 (minimumBy)
import           Formatting                (build, int, sformat, shown, (%))
import           Mockable                  (Async, Catch, Mockable, MonadMockable,
                                            Promise, Throw, bracket, catchAll, fork,
                                            killThread, throw, waitAnyUnexceptional)
import qualified Network.Kademlia          as K
import           Serokell.Util             (ms, sec)
import           System.Directory          (doesFileExist)
import           System.Wlog               (WithLogger, logDebug, logError, logInfo,
                                            logWarning, usingLoggerName)

import           Pos.Binary.Class          (Bi (..))
import           Pos.Binary.Infra.DHTModel ()
import           Pos.DHT.Constants         (enhancedMessageTimeout,
                                            neighborsSendThreshold)
import           Pos.DHT.Model.Class       (DHTException (..), MonadDHT (..),
                                            withDhtLogger)
import           Pos.DHT.Model.Types       (DHTData, DHTKey, DHTNode (..), randomDHTKey)
import           Pos.DHT.Model.Util        (joinNetworkNoThrow)
import           Pos.DHT.Real.Types        (DHTHandle, KademliaDHT (..),
                                            KademliaDHTInstance (..),
                                            KademliaDHTInstanceConfig (..))
import           Pos.Util.TimeLimit        (runWithRandomIntervals')
import           Pos.Util.TimeWarp         (NetworkAddress)


kademliaConfig :: K.KademliaConfig
kademliaConfig = K.defaultConfig { K.k = 16 }

-- | Run KademliaDHT
runKademliaDHT
    :: ( MonadMockable m
       , Eq (Promise m (Maybe ()))
       , MonadIO m
       , WithLogger m
       )
    => KademliaDHTInstance -> KademliaDHT m a -> m a
runKademliaDHT inst action = runReaderT (unKademliaDHT action') inst
  where
    action' = bracket spawnR killThread (const action)
    spawnR = fork $ runWithRandomIntervals' (ms 500) (sec 5) rejoinNetwork

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
startDHTInstance KademliaDHTInstanceConfig {..} = do
    logInfo "Generating dht key.."
    kdiKey <- maybe randomDHTKey pure kdcKey
    logInfo $ sformat ("Generated dht key "%build) kdiKey
    shouldRestore <- liftIO $ doesFileExist kdcDumpPath
    kdiHandle <-
        if shouldRestore
        then do logInfo "Restoring DHT Instance from snapshot"
                catchErrors $
                    createKademliaFromSnapshot kdcPort kademliaConfig =<<
                    decode <$> BS.readFile kdcDumpPath
        else do logInfo "Creating new DHT instance"
                catchErrors $ createKademlia kdcPort kdiKey kademliaConfig

    logInfo "Created DHT instance"
    let kdiInitialPeers = kdcInitialPeers
    let kdiExplicitInitial = kdcExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    pure $ KademliaDHTInstance {..}
  where
    catchErrorsHandler e = do
        logError $ sformat ("Error launching kademlia at port "%int%": "%shown) kdcPort e
        throw e
    catchErrors x = liftIO x `catchAll` catchErrorsHandler

    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText
    createKademlia port key cfg =
        K.createL "127.0.0.1" (fromIntegral port) key cfg (log' logDebug) (log' logError)
    createKademliaFromSnapshot port cfg snapshot =
        K.createLFromSnapshot "127.0.0.1" (fromIntegral port)
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
    => KademliaDHT m ()
rejoinNetwork = withDhtLogger $ do
    init <- KademliaDHT $ asks kdiInitialPeers
    peers <- getKnownPeers
    logDebug $ sformat ("rejoinNetwork: peers " % build) peers
    when (length peers < neighborsSendThreshold) $ do
      logWarning $ sformat ("Not enough peers: "%int%", threshold is "%int)
                           (length peers) (neighborsSendThreshold :: Int)
      joinNetworkNoThrow init

getKnownPeersImpl
    :: ( MonadIO m
       , Mockable Async m
       , Mockable Catch m
       , Mockable Throw m
       , Eq (Promise m (Maybe ()))
       , WithLogger m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaDHT m [DHTNode]
getKnownPeersImpl = do
    myId <- currentNodeKey
    (inst, initialPeers, explicitInitial) <-
        KademliaDHT $
        (,,) <$> ask <*> asks kdiInitialPeers <*> asks kdiExplicitInitial
    buckets <- liftIO (K.viewBuckets $ kdiHandle inst)
    let initPeers = bool [] initialPeers explicitInitial
    filter ((/= myId) . dhtNodeId) <$> extendPeers inst myId initPeers buckets
  where
    extendPeers
        :: MonadIO m1
        => KademliaDHTInstance
        -> DHTKey
        -> [DHTNode]
        -> [[(K.Node DHTKey, Int64)]]
        -> m1 [DHTNode]
    extendPeers inst myId initial buckets =
        map snd .
        HM.toList .
        HM.delete myId .
        flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial .
        HM.fromList . map (\(toDHTNode -> n) -> (dhtNodeId n, n)) <$>
        (updateCache inst $ concatMap getPeersFromBucket buckets)

    getPeersFromBucket
        :: [(K.Node DHTKey, Int64)]
        -> [K.Node DHTKey]
    getPeersFromBucket bucket
        | null bucket = []
        | otherwise =
            let peers = filter ((< enhancedMessageTimeout) . snd) bucket in
            if null peers then
                [fst $ minimumBy (\x y -> compare (snd x) (snd y)) bucket]
            else
                map fst $ peers

    updateCache :: MonadIO m1 => KademliaDHTInstance -> [K.Node DHTKey] -> m1 [K.Node DHTKey]
    updateCache inst peers =
        peers <$ (atomically $ writeTVar (kdiKnownPeersCache inst) peers)

instance ( MonadIO m
         , Mockable Async m
         , Mockable Catch m
         , Mockable Throw m
         , Eq (Promise m (Maybe ()))
         , WithLogger m
         , Bi DHTData
         , Bi DHTKey
         ) =>
         MonadDHT (KademliaDHT m) where
    joinNetwork [] = throw AllPeersUnavailable
    joinNetwork nodes = do
        inst <- KademliaDHT $ asks kdiHandle
        KademliaDHT $
            waitAnyUnexceptional (map (joinNetwork' inst) nodes) >>= handleRes
      where
        handleRes (Just _) = pure ()
        handleRes _        = throw AllPeersUnavailable
    discoverPeers = do
        inst <- KademliaDHT $ asks kdiHandle
        _ <- liftIO $ K.lookupNode inst =<< randomDHTKey
        getKnownPeers
    getKnownPeers = getKnownPeersImpl
    currentNodeKey = KademliaDHT $ asks kdiKey
    dhtLoggerName _ = "kademlia"


toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer K.Peer{..} = (encodeUtf8 peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (decodeUtf8 peerHost) (fromIntegral peerPort)

joinNetwork'
    :: (MonadIO m, Mockable Throw m, WithLogger m, Bi DHTKey, Bi DHTData)
    => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
    let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
    res <- liftIO $ K.joinNetwork inst node'
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throw NodeDown
        K.NodeBanned ->
            logInfo $
            sformat ("joinNetwork: node " % build % " is banned") node
        K.IDClash ->
            logInfo $
            sformat ("joinNetwork: node " % build % " already contains us") node
