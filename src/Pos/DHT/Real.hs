{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.DHT.Real (KademliaDHT, runKademliaDHT) where

import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow,
                                            finally, throwM)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Logging  (WithNamedLogger)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId)
import           Data.Binary               (Binary, decodeOrFail, encode)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Data.Text                 (pack, unpack)
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

instance (MonadIO m, MonadThrow m) => MonadDHT (KademliaDHT m) where

  joinNetwork peers = do
      inst <- KademliaDHT $ asks fst
      asyncs <- mapM (liftIO . async . joinNetwork' inst) peers
      liftIO (waitAnyUnexceptional asyncs) >>= handleRes
    where
      handleRes (Just _) = return ()
      handleRes _ = throwM $ AllPeersUnavailable

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

-- TODO add logging
joinNetwork' :: (MonadIO m, MonadThrow m) => DHTHandle -> Peer -> m ()
joinNetwork' inst peer = do
  let peer' = toKPeer peer
  -- kademlia library has a little bit awkward interface, asking to provide Node instead of Peer (which is necessary), so we provide some random id with arbitrary type
  node <- K.Node peer' <$> randomDHTKey DHTSupporter
  res <- liftIO $ K.joinNetwork inst node
  case res of
    K.JoinSucces -> return ()
    K.NodeDown -> throwM NodeDown
    K.IDClash -> throwM IDClash

-- TODO move to serokell-core ?
waitAnyUnexceptional :: [Async a] -> IO (Maybe (Async a, a))
waitAnyUnexceptional asyncs = waitAnyCatch asyncs >>= handleRes
  where
    handleRes (async', Right res) = return $ Just (async', res)
    handleRes (async', Left _) = if null asyncs'
                                    then return Nothing
                                    else waitAnyUnexceptional asyncs'
      where asyncs' = filter (/= async') asyncs
