{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.NewDHT.Real.Real
       ( runKademliaDHT
       , startDHTInstance
       , stopDHTInstance

       -- * For Servant integration
       , getKademliaDHTCtx
       , runKademliaDHTRaw
       ) where

import           Control.Concurrent.Async.Lifted (async, mapConcurrently)
import           Control.Concurrent.STM          (STM, TVar, modifyTVar, newTVar,
                                                  readTVar, swapTVar, writeTVar)
import           Control.Monad.Catch             (Handler (..), MonadCatch, MonadMask,
                                                  MonadThrow, catchAll, catches, throwM)
import           Control.TimeWarp.Rpc            (Binding (..), NetworkAddress,
                                                  RawData (..), TransferException (..),
                                                  listenR, sendH, sendR)
import           Control.TimeWarp.Timed          (ms, sec)
import           Mockable                        (MonadMockable, fork, killThread,
                                                  newSharedAtomic)

import qualified Data.Cache.LRU                  as LRU
import           Data.Hashable                   (hash)
import qualified Data.HashMap.Strict             as HM
import           Data.List                       (intersect, (\\))

import           Formatting                      (build, int, sformat, shown, (%))
import qualified Network.Kademlia                as K
import           Prelude                         (id)
import           System.Wlog                     (WithLogger, getLoggerName, logDebug,
                                                  logError, logInfo, logWarning,
                                                  usingLoggerName)
import           Universum                       hiding (async, fromStrict,
                                                  mapConcurrently, toStrict)

import           Pos.Binary.Class                (Bi (..))
import           Pos.Binary.NewDHTModel          ()
import           Pos.Constants                   (enchancedMessageBroadcast)
import           Pos.Constants                   (neighborsSendThreshold)
import           Pos.NewDHT.Model.Class          (DHTException (..), MonadDHT (..),
                                                  withDhtLogger)
import           Pos.NewDHT.Model.Types          (DHTData, DHTKey, DHTNode (..),
                                                  addressToNodeId, filterByNodeType,
                                                  randomDHTKey)
import           Pos.NewDHT.Model.Util           (joinNetworkNoThrow)
import           Pos.NewDHT.Real.Types           (DHTHandle, KademliaDHT (..),
                                                  KademliaDHTConfig (..),
                                                  KademliaDHTContext (..),
                                                  KademliaDHTInstance (..),
                                                  KademliaDHTInstanceConfig (..))
import           Pos.Util                        (runWithRandomIntervals',
                                                  waitAnyUnexceptional)

import           Node                            (node)

kademliaConfig :: K.KademliaConfig
kademliaConfig = K.defaultConfig { K.k = 16 }

-- | Run 'KademliaDHT' with provided 'KademliaDHTContext'
runKademliaDHTRaw :: KademliaDHTContext m -> KademliaDHT m a -> m a
runKademliaDHTRaw ctx action = runReaderT (unKademliaDHT action) ctx

-- | Get context from 'KademliaDHT'
getKademliaDHTCtx :: Monad m => KademliaDHT m (KademliaDHTContext m)
getKademliaDHTCtx = KademliaDHT ask

-- | Run 'KademliaDHT' with provided 'KademliaDTHConfig'.
runKademliaDHT
    :: ( WithLogger m
       , MonadIO m
       , MonadMockable m
       , MonadMask m
       )
    => KademliaDHTConfig m -> KademliaDHT m a -> m a
runKademliaDHT kdc@(KademliaDHTConfig {..}) action =
    startDHT kdc >>= runReaderT (unKademliaDHT action')
  where
    action' =
        action''
        `finally`
        (logDebug "stopping kademlia messager"
          >> stopDHT >> logDebug "kademlia messager stopped")
    action'' = do
      logDebug "running kademlia dht messager"
      joinNetworkNoThrow (kdiInitialPeers $ kdcDHTInstance)
      startRejoinThread
      action
    startRejoinThread = do
      tvar <- KademliaDHT $ asks kdcAuxClosers
      tid <- fork $ runWithRandomIntervals' (ms 500) (sec 5) rejoinNetwork
      liftIO $ atomically $ modifyTVar tvar (killThread tid:)

-- | Stop DHT algo.
stopDHT :: (MonadIO m, MonadMockable m) => KademliaDHT m ()
stopDHT = do
    (closersTV, stoppedTV) <- KademliaDHT $ (,)
            <$> asks kdcAuxClosers
            <*> asks kdcStopped
    atomically $ writeTVar stoppedTV True
    closers <- atomically $ swapTVar closersTV []
    sequence_ closers

-- | Stop chosen 'KademliaDHTInstance'.
stopDHTInstance
    :: MonadIO m
    => KademliaDHTInstance -> m ()
stopDHTInstance KademliaDHTInstance {..} = liftIO $ K.close kdiHandle

-- | Start 'KademliaDHTInstance' with 'KademliaDHTInstanceConfig'.
startDHTInstance
    :: ( MonadIO m
       , MonadMockable m
       , WithLogger m
       , MonadCatch m
       , Bi DHTData
       , Bi DHTKey
       )
    => KademliaDHTInstanceConfig -> m KademliaDHTInstance
startDHTInstance KademliaDHTInstanceConfig {..} = do
    kdiKey <- either pure randomDHTKey kdcKeyOrType
    kdiHandle <-
        (liftIO $
        K.createL
            (fromInteger . toInteger $ kdcPort)
            kdiKey
            kademliaConfig
            (log' logDebug)
            (log' logError))
          `catchAll`
        (\e ->
           do logError $ sformat
                  ("Error launching kademlia at port " % int % ": " % shown) kdcPort e
              throwM e)
    let kdiInitialPeers = kdcInitialPeers
    let kdiExplicitInitial = kdcExplicitInitial
    kdiKnownPeersCache <- atomically $ newTVar []
    pure $ KademliaDHTInstance {..}
  where
    log' logF =  usingLoggerName ("kademlia" <> "messager") . logF . toText

startDHT
    :: ( MonadIO m
       , MonadMockable m
       , WithLogger m
       , MonadMask m
       )
    => KademliaDHTConfig m -> m (KademliaDHTContext m)
startDHT KademliaDHTConfig {..} = do
    kdcStopped <- liftIO $ atomically $ newTVar False
    kdcNode <- notImplemented
    let kdcDHTInstance_ = kdcDHTInstance
    kdcAuxClosers <- liftIO $ atomically $ newTVar mempty
    pure $ KademliaDHTContext {..}

rejoinNetwork
    :: (MonadIO m, WithLogger m, MonadCatch m, Bi DHTData, Bi DHTKey)
    => KademliaDHT m ()
rejoinNetwork = withDhtLogger $ do
    peers <- getKnownPeers
    logDebug $ sformat ("rejoinNetwork: peers " % build) peers
    when (length peers < neighborsSendThreshold) $ do
      logWarning $ sformat ("Not enough peers: "%int%", threshold is "%int)
                           (length peers) neighborsSendThreshold
      init <- KademliaDHT $ asks (kdiInitialPeers . kdcDHTInstance_)
      joinNetworkNoThrow init

instance (MonadIO m, MonadCatch m, WithLogger m, Bi DHTData, Bi DHTKey) =>
         MonadDHT (KademliaDHT m) where
    joinNetwork [] = throwM AllPeersUnavailable
    joinNetwork nodes = do
        inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
        loggerName <- getLoggerName
        asyncs <-
            mapM
                (liftIO . usingLoggerName loggerName . async . joinNetwork' inst)
                nodes
        waitAnyUnexceptional asyncs >>= handleRes
      where
        handleRes (Just _) = pure ()
        handleRes _        = throwM AllPeersUnavailable
    discoverPeers type_ = do
        inst <- KademliaDHT $ asks (kdiHandle . kdcDHTInstance_)
        _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
        filterByNodeType type_ <$> getKnownPeers
    getKnownPeers = do
        myId <- currentNodeKey
        (inst, initialPeers, explicitInitial) <-
            KademliaDHT $
            (,,) <$> asks kdcDHTInstance_ <*>
            asks (kdiInitialPeers . kdcDHTInstance_) <*>
            asks (kdiExplicitInitial . kdcDHTInstance_)
        extendPeers
          inst
          myId
          (if explicitInitial
            then initialPeers
            else []) =<<
          liftIO (K.dumpPeers $ kdiHandle inst)
      where
        extendPeers inst myId initial peers =
            map snd .
            HM.toList .
            HM.delete myId .
            flip (foldr $ \n -> HM.insert (dhtNodeId n) n) initial .
            HM.fromList . map (\(toDHTNode -> n) -> (dhtNodeId n, n)) <$>
            (updateCache inst =<<
            selectSufficientNodes inst myId peers)

        selectSufficientNodes inst myId l =
            if enchancedMessageBroadcast /= (0 :: Int)
            then concat <$> mapM (getPeersFromBucket enchancedMessageBroadcast inst)
                                 (splitToBuckets (kdiHandle inst) myId l)
            else return l

        bucketIndex origin x = length . takeWhile (not . id) <$> K.distance origin (K.nodeId x)

        insertId origin i hm = do
            bucket <- bucketIndex origin i
            return $ HM.insertWith (++) bucket [i] hm

        splitToBuckets kInst origin peers = flip K.usingKademliaInstance kInst $
            HM.elems <$> foldrM (insertId origin) HM.empty peers

        getPeersFromBucket p inst bucket = do
            cache <- atomically $ readTVar $ kdiKnownPeersCache inst
            let fromCache = tryTake p $ intersect cache bucket
            return $ fromCache ++ tryTake (p - length fromCache) (bucket \\ cache)

        tryTake x l = if length l < x then l else take x l

        updateCache inst peers = do
            atomically $ writeTVar (kdiKnownPeersCache inst) peers
            return peers

    currentNodeKey = KademliaDHT $ asks (kdiKey . kdcDHTInstance_)
    dhtLoggerName _ = "kademlia"

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer K.Peer{..} = (encodeUtf8 peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (decodeUtf8 peerHost) (fromIntegral peerPort)

joinNetwork'
    :: (MonadIO m, MonadThrow m, WithLogger m, Bi DHTKey, Bi DHTData)
    => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
    let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
    res <- liftIO $ K.joinNetwork inst node'
    case res of
        K.JoinSuccess -> pure ()
        K.NodeDown -> throwM NodeDown
        K.NodeBanned ->
            logInfo $
            sformat ("joinNetwork: node " % build % " is banned") node
        K.IDClash ->
            logInfo $
            sformat ("joinNetwork: node " % build % " already contains us") node
