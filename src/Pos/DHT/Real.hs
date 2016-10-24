{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.DHT.Real (KademliaDHT, runKademliaDHT, KademliaDHTConfig (..)) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow, finally,
                                            throwM)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Logging  (WithNamedLogger, logWarning)
import           Control.TimeWarp.Rpc      (Listener (..), Message, MonadDialog,
                                            MonadTransfer, listen, send, Binding (..))
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId, fork, killThread)
import           Data.Binary               (Binary, decodeOrFail, encode)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.Text                 (pack, unpack)
import           Formatting                (sformat, shown, (%))
import qualified Network.Kademlia          as K
import           Pos.DHT                   (DHTData, DHTException (..), DHTKey,
                                            DHTNode (..), DHTNodeType (..),
                                            MonadBroadcast (..), MonadDHT (..), Peer (..),
                                            dhtNodeType, randomDHTKey)
import           Universum                 hiding (ThreadId, finally, fromStrict,
                                            killThread, toStrict)
--import Data.Data (Data)
-- import Data.Hashable (hash)

import           Control.Concurrent.STM    (TVar, atomically, newTVar, readTVar,
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
                                               , kdcMsgThreadId :: ThreadId m
                                               }

data KademliaDHTConfig m = KademliaDHTConfig
                            { kdcType             :: DHTNodeType
                            , kdcPort             :: Word16
                            , kdcListeners        :: [Listener (KademliaDHT m)]
                            , kdcMessageCacheSize :: Int
                            }

newtype KademliaDHT m a = KademliaDHT (ReaderT (KademliaDHTContext m) m a)
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO,
             MonadMask, WithNamedLogger, MonadTimed, MonadTransfer, MonadDialog)

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift

type instance ThreadId (KademliaDHT m) = ThreadId m

runKademliaDHT :: (MonadIO m, MonadTimed m, MonadDialog m, MonadMask m) => KademliaDHTConfig m -> KademliaDHT m a -> m a
runKademliaDHT kdc (KademliaDHT action) = do
  ctx <- startDHT kdc
  runReaderT action ctx `finally` stopDHT ctx

stopDHT :: (MonadTimed m, MonadDialog m, MonadIO m) => KademliaDHTContext m -> m ()
stopDHT ctx = do
  liftIO $ K.close $ kdcHandle ctx
  killThread $ kdcMsgThreadId ctx

startDHT :: (MonadTimed m, MonadDialog m, MonadIO m) => KademliaDHTConfig m -> m (KademliaDHTContext m)
startDHT (KademliaDHTConfig {..}) = do
  kdcKey <- randomDHTKey kdcType
  kdcHandle <- liftIO $ K.create (fromInteger . toInteger $ kdcPort) kdcKey
  kdcMsgCache <- liftIO . atomically $ newTVar (LRU.newLRU (Just $ toInteger kdcMessageCacheSize) :: LRU.LRU Int ())
  kdcMsgThreadId <- fork $ listen (AtPort kdcPort) []
  return $ KademliaDHTContext {..}

--msgListeners :: (MonadTimed m, MonadDialog m, MonadIO m, MonadBroadcast m) => TVar (LRU.LRU Int ()) -> [Listener m] -> [Listener m]
--msgListeners cacheTV = map (\Listener f -> Listener $ impl f)
--  where
--    impl f (BroadcastMessage m) = do
--      isNew <- atomically $ do
--        cache <- readTVar cacheTV
--        let mHash = hash $ encode m
--            (cache', mP) = mHash `LRU.lookup` cache
--        case mP of
--          Just _ -> writeTVar cacheTV cache' >> return False
--          _ -> writeTVar cacheTV (LRU.insert mHash () cache') >> return True
--      when isNew $ do
--        --sendBroadcast m
--        f m

-- data DHTMessage r = BroadcastMessage r | SimpleMessage r
--   deriving (Typeable, Data)
--
-- instance Message r => Message (DHTMessage r)

instance (MonadDialog m, WithNamedLogger m, MonadThrow m, MonadIO m) => MonadBroadcast (KademliaDHT m) where
  sendBroadcast = notImplemented
  --sendBroadcast msg = do
  --    known <- fmap toAddress <$> getKnownPeers
  --    mapM_ send' known
  --  where
  --    send' addr = send addr msg `catch` logInfo (sformat ("Failed to send message to " % build) addr)

instance (MonadIO m, MonadThrow m, WithNamedLogger m) => MonadDHT (KademliaDHT m) where

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
    _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
    filter (\n -> dhtNodeType (dhtNodeId n) == Just type_) <$> getKnownPeers

  getKnownPeers = do
    inst <- KademliaDHT $ asks kdcHandle
    fmap toDHTNode <$> liftIO (K.dumpPeers inst)

  currentNodeKey = KademliaDHT $ asks kdcKey

toDHTNode :: K.Node DHTKey -> DHTNode
toDHTNode n = DHTNode (fromKPeer . K.peer $ n) $ K.nodeId n

fromKPeer :: K.Peer -> Peer
fromKPeer (K.Peer {..}) = Peer (pack peerHost) (fromInteger . toInteger $ peerPort)

toKPeer :: Peer -> K.Peer
toKPeer (Peer {..}) = K.Peer (unpack peerHost) (fromInteger . toInteger $ peerPort)

-- TODO add TimedIO, WithLoggerName constraints and uncomment logging
joinNetwork' :: (MonadIO m, MonadThrow m) => DHTHandle -> DHTNode -> m ()
joinNetwork' inst node = do
  let node' = K.Node (toKPeer $ dhtPeer node) (dhtNodeId node)
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
