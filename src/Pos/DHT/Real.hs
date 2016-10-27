{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeFamilies          #-}

module Pos.DHT.Real
       ( KademliaDHT
       , runKademliaDHT
       , KademliaDHTConfig(..)
       ) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow, finally,
                                            throwM)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Logging  (WithNamedLogger (getLoggerName), logDebug,
                                            logError, logInfo, logWarning,
                                            usingLoggerName)
import           Control.TimeWarp.Rpc      (Binding (..), ListenerH (..), MonadDialog,
                                            MonadResponse, MonadTransfer, NetworkAddress,
                                            RawData, listenR, sendH, sendR)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId, fork, killThread)
import           Data.Binary               (Binary, Put, decodeOrFail, encode, get, put)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.Hashable             (hash)
import           Formatting                (int, sformat, shown, (%))
import qualified Network.Kademlia          as K
import           Pos.DHT                   (DHTData, DHTException (..), DHTKey,
                                            DHTNode (..), DHTNodeType (..),
                                            ListenerDHT (..), MonadDHT (..),
                                            MonadMessageDHT (..),
                                            WithDefaultMsgHeader (..), filterByNodeType,
                                            getDHTResponseT, joinNetworkNoThrow,
                                            randomDHTKey, withDhtLogger)
import           Universum                 hiding (ThreadId, finally, fromStrict,
                                            killThread, toStrict)
--import Data.Data (Data)
-- import Data.Hashable (hash)

import           Control.Concurrent.STM    (STM, TVar, atomically, newTVar, readTVar,
                                            writeTVar)
import qualified Data.Cache.LRU            as LRU

toBSBinary :: Binary b => b -> BS.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Binary b => BS.ByteString -> Either [Char] (b, BS.ByteString)
fromBSBinary bs = case decodeOrFail $ fromStrict bs of
                Left (_, _, errMsg)  -> Left errMsg
                Right (rest, _, res) -> Right (res, toStrict rest)

instance K.Serialize DHTData where
  toBS = toBSBinary
  fromBS = fromBSBinary

instance K.Serialize DHTKey where
  toBS = toBSBinary
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

data KademliaDHTContext m = KademliaDHTContext { kdcHandle      :: DHTHandle
                                               , kdcKey         :: DHTKey
                                               , kdcMsgThreadId :: TVar (Maybe (ThreadId (KademliaDHT m)))
                                               , kdcInitialPeers_ :: [DHTNode]
                                               }

data KademliaDHTConfig m = KademliaDHTConfig
                            { kdcPort             :: Word16
                            , kdcListeners        :: [ListenerDHT (KademliaDHT m)]
                            , kdcMessageCacheSize :: Int
                            , kdcEnableBroadcast  :: Bool
                            , kdcKeyOrType        :: Either DHTKey DHTNodeType
                            , kdcInitialPeers     :: [DHTNode]
                            }

newtype KademliaDHT m a = KademliaDHT { unKademliaDHT :: ReaderT (KademliaDHTContext m) m a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO,
             MonadMask, WithNamedLogger, MonadTimed, MonadTransfer, MonadDialog, MonadResponse)

instance Applicative m => WithDefaultMsgHeader (KademliaDHT m) where
  defaultMsgHeader _ = pure $ put SimpleHeader

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift

type instance ThreadId (KademliaDHT m) = ThreadId m

runKademliaDHT
    :: (WithNamedLogger m, MonadIO m, MonadTimed m, MonadDialog m, MonadMask m)
    => KademliaDHTConfig m -> KademliaDHT m a -> m a
runKademliaDHT kdc@(KademliaDHTConfig {..}) action = do
    ctx <- startDHT kdc
    runReaderT (unKademliaDHT $ action' ctx) ctx
  where
    action' ctx = (startMsgThread (kdcMsgThreadId ctx) >> action'') `finally` stopDHT ctx
    action'' = do
      joinNetworkNoThrow kdcInitialPeers
      action
    startMsgThread tvar = do
      msgCache <- liftIO . atomically $ newTVar (LRU.newLRU (Just $ toInteger kdcMessageCacheSize) :: LRU.LRU Int ())
      tId <- fork $ listenR (AtPort kdcPort) get (convert <$> kdcListeners) (rawListener kdcEnableBroadcast msgCache)
      liftIO . atomically $ writeTVar tvar (Just tId)
    convert (ListenerDHT f) = ListenerH $ \(_, m) -> getDHTResponseT $ f m

stopDHT :: (MonadTimed m, MonadIO m) => KademliaDHTContext m -> KademliaDHT m ()
stopDHT ctx = do
    liftIO $ K.close $ kdcHandle ctx
    mThreadId <- liftIO . atomically $ do
      tId <- readTVar $ kdcMsgThreadId ctx
      writeTVar (kdcMsgThreadId ctx) Nothing
      return tId
    case mThreadId of
      Just tid -> killThread tid
      _        -> return ()

startDHT :: (MonadTimed m, MonadIO m, WithNamedLogger m) => KademliaDHTConfig m -> m (KademliaDHTContext m)
startDHT (KademliaDHTConfig {..}) = do
    kdcKey <- either return randomDHTKey kdcKeyOrType
    kdcHandle <- liftIO $ K.create (fromInteger . toInteger $ kdcPort) kdcKey (log' logDebug) (log' logError)
    kdcMsgThreadId <- liftIO . atomically $ newTVar Nothing
    let kdcInitialPeers_ = kdcInitialPeers
    return $ KademliaDHTContext {..}
  where
    log' log =  usingLoggerName ("kademlia" <> "instance") . log . toS

rawListener
    :: (MonadIO m, MonadDHT m, MonadDialog m, WithNamedLogger m)
    => Bool -> TVar (LRU.LRU Int ()) -> (DHTMsgHeader, RawData) -> m Bool
rawListener enableBroadcast cache (h, rawData) = withDhtLogger $ do
    let mHash = hash $ encode rawData
    logDebug $
        sformat ("Received message (" % shown % ", hash=" % int) h mHash
    wasInCache <- liftIO . atomically $ updCache cache mHash
    wasInCache <$
        (unless wasInCache $
        case h of
            BroadcastHeader -> when enableBroadcast $ sendToNetworkR rawData
            SimpleHeader    -> pure ())

updCache :: TVar (LRU.LRU Int ()) -> Int -> STM Bool
updCache cacheTV dataHash = do
    cache <- readTVar cacheTV
    let (cache', mP) = dataHash `LRU.lookup` cache
    case mP of
      Just _ -> writeTVar cacheTV cache' >> return True
      _      -> writeTVar cacheTV (LRU.insert dataHash () cache') >> return False

sendToNetworkR :: (MonadDialog m, MonadDHT m) => RawData -> m ()
sendToNetworkR = sendToNetworkImpl sendR

sendToNetworkImpl :: (MonadDialog m, MonadDHT m) => (NetworkAddress -> Put -> msg -> m ()) -> msg -> m ()
sendToNetworkImpl = notImplemented

data DHTMsgHeader = BroadcastHeader
                  | SimpleHeader
  deriving (Generic, Show)

instance Binary DHTMsgHeader

instance (MonadDialog m, WithNamedLogger m, MonadCatch m, MonadIO m) => MonadMessageDHT (KademliaDHT m) where
  sendToNetwork = sendToNetworkImpl sendH

instance (MonadIO m, MonadCatch m, WithNamedLogger m) => MonadDHT (KademliaDHT m) where

  joinNetwork [] = throwM AllPeersUnavailable
  joinNetwork nodes = do
      inst <- KademliaDHT $ asks kdcHandle
      asyncs <- mapM (liftIO . async . joinNetwork' inst) nodes
      waitAnyUnexceptional asyncs >>= handleRes
    where
      handleRes (Just _) = return ()
      handleRes _        = throwM AllPeersUnavailable

  discoverPeers type_ = do
    inst <- KademliaDHT $ asks kdcHandle
    peers <- getKnownPeers
    when (null peers) $ do
      logWarning "Empty known peer list"
      init <- KademliaDHT $ asks kdcInitialPeers_
      joinNetworkNoThrow init
    _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
    filterByNodeType type_ <$> getKnownPeers

  getKnownPeers = do
    myId <- currentNodeKey
    inst <- KademliaDHT $ asks kdcHandle
    filter (\n -> dhtNodeId n /= myId) . fmap toDHTNode <$> liftIO (K.dumpPeers inst)

  currentNodeKey = KademliaDHT $ asks kdcKey

  dhtLoggerName _ = "kademlia"

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> NetworkAddress
fromKPeer (K.Peer {..}) = (toS peerHost, fromIntegral peerPort)

toKPeer :: NetworkAddress -> K.Peer
toKPeer (peerHost, peerPort) = K.Peer (toS peerHost) (fromIntegral peerPort)

-- TODO add TimedIO, WithLoggerName constraints and uncomment logging
joinNetwork' :: (MonadIO m, MonadThrow m) => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
  let node' = K.Node (toKPeer $ dhtAddr node) (dhtNodeId node)
  res <- liftIO $ K.joinNetwork inst node'
  case res of
    K.JoinSucces -> return ()
    K.NodeDown   -> throwM NodeDown
    K.IDClash    -> return () --logInfo $ sformat ("joinNetwork: node " % build % " already contains us") node

-- TODO move to serokell-core ?
waitAnyUnexceptional :: (MonadIO m, WithNamedLogger m) => [Async a] -> m (Maybe (Async a, a))
waitAnyUnexceptional asyncs = liftIO (waitAnyCatch asyncs) >>= handleRes
  where
    handleRes (async', Right res) = return $ Just (async', res)
    handleRes (async', Left e) = do
      logWarning $ sformat ("waitAnyUnexceptional: caught error " % shown) e
      if null asyncs'
         then return Nothing
         else waitAnyUnexceptional asyncs'
      where asyncs' = filter (/= async') asyncs
