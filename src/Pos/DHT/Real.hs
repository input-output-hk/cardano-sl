{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.DHT.Real (KademliaDHT, runKademliaDHT) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow,
                                            finally, throwM)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Logging  (WithNamedLogger, logWarning)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId)
import           Data.Binary               (Binary, decodeOrFail, encode)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.Text                 (pack, unpack)
import           Formatting                (sformat, shown, (%))
import qualified Network.Kademlia          as K
import           Pos.DHT                   (DHTData, DHTException (..), DHTKey,
                                            DHTNode (..), DHTNodeType (..),
                                            MonadDHT (..), Peer (..),
                                            dhtNodeType, randomDHTKey)
import           Universum                 hiding (ThreadId, finally,
                                            fromStrict, toStrict)

toBSBinary :: Binary b => b -> BS.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Binary b => BS.ByteString -> Either [Char] (b, BS.ByteString)
fromBSBinary bs = case decodeOrFail $ fromStrict bs of
                Left (_, _, errMsg) -> Left errMsg
                Right (rest, _, res) -> Right (res, toStrict rest)

instance K.Serialize DHTData where
  toBS = toBSBinary
  fromBS = fromBSBinary

instance K.Serialize DHTKey where
  toBS = toBSBinary
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

type KademliaDHTContext = (DHTHandle, DHTKey)

newtype KademliaDHT m a = KademliaDHT (ReaderT KademliaDHTContext m a)
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO, MonadMask, WithNamedLogger, MonadTimed, MonadTrans)

type instance ThreadId (KademliaDHT m) = ThreadId m

runKademliaDHT :: (MonadIO m, MonadMask m) => DHTNodeType -> Word16 -> KademliaDHT m a -> m a
runKademliaDHT type_ port (KademliaDHT action) = do
  (dht, key) <- startDHT type_ port
  runReaderT action (dht, key) `finally` stopDHT dht

stopDHT :: MonadIO m => DHTHandle -> m ()
stopDHT inst = liftIO $ K.close inst

startDHT :: MonadIO m => DHTNodeType -> Word16 -> m (DHTHandle, DHTKey)
startDHT type_ port = do
  key <- randomDHTKey type_
  liftIO $ (, key) <$> K.create (fromInteger . toInteger $ port) key

instance (MonadIO m, MonadThrow m, WithNamedLogger m) => MonadDHT (KademliaDHT m) where

  joinNetwork [] = throwM AllPeersUnavailable
  joinNetwork nodes = do
      inst <- KademliaDHT $ asks fst
      asyncs <- mapM (liftIO . async . joinNetwork' inst) nodes
      waitAnyUnexceptional asyncs >>= handleRes
    where
      handleRes (Just _) = return ()
      handleRes _ = throwM AllPeersUnavailable

  discoverPeers type_ = do
    inst <- KademliaDHT $ asks fst
    _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
    filter (\n -> dhtNodeType (dhtNodeId n) == Just type_) <$> getKnownPeers

  getKnownPeers = do
    inst <- KademliaDHT $ asks fst
    fmap toDHTNode <$> liftIO (K.dumpPeers inst)

  currentNodeKey = KademliaDHT $ asks snd

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
    K.NodeDown -> throwM NodeDown
    K.IDClash -> return () --logInfo $ sformat ("joinNetwork: node " % build % " already contains us") node


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
