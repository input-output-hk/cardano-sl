-- | Peer discovery

module Pos.DHT (
  DHTException (..), DHTHandle, DHTKey, DHTNode, Peer,
  peerPort, peerHost, mkPeer, dhtPeer, dhtNodeId,
  randomDHTKey, startDHT, getKnownPeers, stopDHT, joinNetwork
) where

import           Crypto.Random    (secureRandomBS)
import qualified Data.ByteString  as BS
import qualified Network.Kademlia as K
import           Universum

newtype Peer = Peer { getPeer_ :: K.Peer }

peerPort :: Peer -> Word16
peerPort = K.peerPort . getPeer_

peerHost :: Peer -> String
peerHost = K.peerHost . getPeer_

mkPeer :: String -> Word16 -> Peer
mkPeer host port = Peer $ K.Peer host $ fromInteger port

newtype DHTData = DHTData ()

instance K.Serialize DHTData where
  toBS = const BS.empty
  fromBS = const . Right $ DHTData ()

newtype DHTKey = DHTKey BS.ByteString

instance K.Serialize DHTKey where
  toBS = id
  fromBS = Right

newtype DHTNode = DHTNode { getNode :: K.Node DHTKey }

dhtPeer :: DHTNode -> Peer
dhtPeer = K.peer . getNode

dhtNodeId :: DHTNode -> DHTKey
dhtNodeId = K.nodeId . getNode

randomDHTKey :: MonadIO m => m DHTKey
randomDHTKey = DHTKey $ secureRandomBS 20

newtype DHTHandle = DHTHandle (K.KademliaInstance DHTKey DHTData)

startDHT :: MonadIO m => Word16 -> m DHTHandle
startDHT port = randomDHTKey >>= liftIO . K.create port

getKnownPeers :: MonadIO m => DHTHandle -> m [DHTNode]
getKnownPeers (DHTHandle inst) = map DHTNode <$> liftIO (K.dumpPeers inst)

-- Peer discovery: query DHT for random key
-- Processing request, node will discover few other nodes
-- We return these newly discovered nodes among with already known
-- (List of known nodes is updated as well)
discoverPeers :: MonadIO m => DHTHandle -> m [DHTNode]
discoverPeers handle@(DHTHandle inst) = do
  (DHTKey rId) <- randomDHTKey
  liftIO $ L.lookup inst rId
  getKnownPeers handle

stopDHT :: MonadIO m => DHTHandle -> m ()
stopDHT (DHTHandle inst) = liftIO $ K.close inst

data DHTException = IDClash | NodeDown
  deriving (Show, Typeable)

instance Exception DHTException

joinNetwork :: (MonadIO m, MonadThrow m) => DHTHandle -> Peer -> m ()
joinNetwork (DHTHandle inst) peer@(Peer peer') = do
  -- kademlia library has a little bit awkward interface, asking to provide Node instead of Peer (which is necessary), so we provide some random id
  (DHTKey rId) <- randomDHTKey
  let node = K.Node peer' rId
  res <- K.joinNetwork inst node
  case res of
    K.JoinSuccess -> return ()
    K.NodeDown -> throwM NodeDown
    K.IDClash -> throwM IDClash

