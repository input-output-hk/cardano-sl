-- | Peer discovery

module Pos.DHT (
  DHTException (..), DHTHandle, DHTKey, DHTNode, Peer,
  peerPort, peerHost, mkPeer, dhtPeer, dhtNodeId,
  randomDHTKey,
  startDHT, getKnownPeers, stopDHT, joinNetwork, discoverPeers
) where

import           Control.Monad.Catch (MonadThrow, throwM)
import qualified Data.ByteString     as BS
import           Data.String         (fromString)
import qualified Network.Kademlia    as K
import           Pos.Crypto.Random   (secureRandomBS)
import           Universum

newtype Peer = Peer { getPeer_ :: K.Peer }

peerPort :: Integral p => Peer -> p
peerPort = fromInteger . toInteger . K.peerPort . getPeer_

peerHost :: IsString s => Peer -> s
peerHost = fromString . K.peerHost . getPeer_

mkPeer :: Integral p => [Char] -> p -> Peer
mkPeer host port = Peer $ K.Peer host $ (fromInteger . toInteger) port

newtype DHTData = DHTData ()
  deriving (Eq, Ord)

instance K.Serialize DHTData where
  toBS = const BS.empty
  fromBS bs = Right (DHTData (), bs)

-- DHTKey should be strictly 20-byte long
newtype DHTKey = DHTKey BS.ByteString
  deriving (Eq, Ord)

instance K.Serialize DHTKey where
  toBS (DHTKey bs) = bs
  fromBS bs = if BS.length keyBS /= 20
                 then Left "Key length less than 20 bytes"
                 else Right (DHTKey keyBS, rest)
    where
      (keyBS, rest) = BS.splitAt 20 bs

newtype DHTNode = DHTNode { getNode :: K.Node DHTKey }

-- Node type is determined by first byte of key
data DHTNodeType
  -- node which participates only in supporting DHT, i.e. not a part of PoS communication
  = DHTSupporter
  -- full node, i.e. fully participating in both DHT supporting and PoS
  | DHTFull
  -- client node (for SPV). Key idea is that clients, being a part of DHT, are rarely queried
  | DHTClient

dhtPeer :: DHTNode -> Peer
dhtPeer = Peer . K.peer . getNode

dhtNodeId :: DHTNode -> DHTKey
dhtNodeId = K.nodeId . getNode

typeByte :: DHTNodeType -> Word8
typeByte DHTSupporter = 0x00
typeByte DHTFull = 0x30
typeByte DHTClient = 0xF0

randomDHTKey :: MonadIO m => DHTNodeType -> m DHTKey
randomDHTKey type_ = (DHTKey . BS.cons (typeByte type_)) <$> secureRandomBS 19

newtype DHTHandle = DHTHandle (K.KademliaInstance DHTKey DHTData)

startDHT :: (Integral p, MonadIO m) => DHTNodeType -> p  -> m DHTHandle
startDHT type_ port = fmap DHTHandle $ randomDHTKey type_ >>= liftIO . K.create (fromInteger . toInteger $ port)

getKnownPeers :: MonadIO m => DHTHandle -> m [DHTNode]
getKnownPeers (DHTHandle inst) = map DHTNode <$> liftIO (K.dumpPeers inst)

-- Peer discovery: query DHT for random key
-- Processing request, node will discover few other nodes
-- We return these newly discovered nodes among with already known
-- (List of known nodes is updated as well)
discoverPeers :: MonadIO m => DHTNodeType -> DHTHandle -> m [DHTNode]
discoverPeers type_ h@(DHTHandle inst) = do
  _ <- liftIO $ K.lookup inst =<< randomDHTKey type_
  getKnownPeers h

stopDHT :: MonadIO m => DHTHandle -> m ()
stopDHT (DHTHandle inst) = liftIO $ K.close inst

data DHTException = IDClash | NodeDown
  deriving (Show, Typeable)

instance Exception DHTException

joinNetwork :: (MonadIO m, MonadThrow m) => DHTHandle -> Peer -> m ()
joinNetwork (DHTHandle inst) (Peer peer') = do
  -- kademlia library has a little bit awkward interface, asking to provide Node instead of Peer (which is necessary), so we provide some random id with arbitrary type
  node <- K.Node peer' <$> randomDHTKey DHTSupporter
  res <- liftIO $ K.joinNetwork inst node
  case res of
    K.JoinSucces -> return ()
    K.NodeDown -> throwM NodeDown
    K.IDClash -> throwM IDClash

