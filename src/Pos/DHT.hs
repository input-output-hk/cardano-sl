{-# LANGUAGE FlexibleInstances #-}
-- | Peer discovery

module Pos.DHT (
  DHTException (..), DHTKey, DHTData, DHTNode (..), Peer (..), DHTNodeType (..),
  MonadDHT (..),
  randomDHTKey, dhtNodeType
) where

import           Control.TimeWarp.Rpc (ResponseT)
import           Data.Binary          (Binary)
import qualified Data.ByteString      as BS
import           Data.Text.Buildable  (Buildable (..))
import           Pos.Crypto.Random    (secureRandomBS)
import           Serokell.Util.Text   (listBuilderJSON)
import           Universum

data Peer = Peer { peerHost :: Text
                 , peerPort :: Word16
                 }
  deriving Show

instance Buildable Peer where
  build p = build ("Peer " :: Text)
             `mappend` build (peerHost p)
             `mappend` build ':'
             `mappend` build (peerPort p)

instance Buildable [Peer] where
  build = listBuilderJSON

newtype DHTData = DHTData ()
  deriving (Eq, Ord, Binary)

-- DHTKey should be strictly 20-byte long
newtype DHTKey = DHTKey BS.ByteString
  deriving (Eq, Ord, Binary)

-- Node type is determined by first byte of key
data DHTNodeType
  -- node which participates only in supporting DHT, i.e. not a part of PoS communication
  = DHTSupporter
  -- full node, i.e. fully participating in both DHT supporting and PoS
  | DHTFull
  -- client node (for SPV). Key idea is that clients, being a part of DHT, are rarely queried
  | DHTClient

dhtNodeType :: DHTKey -> Maybe DHTNodeType
dhtNodeType (DHTKey bs) = impl $ BS.head bs
  where
    impl 0x00 = Just DHTSupporter
    impl 0x30 = Just DHTFull
    impl 0xF0 = Just DHTSupporter
    impl _ = Nothing

typeByte :: DHTNodeType -> Word8
typeByte DHTSupporter = 0x00
typeByte DHTFull = 0x30
typeByte DHTClient = 0xF0

randomDHTKey :: MonadIO m => DHTNodeType -> m DHTKey
randomDHTKey type_ = (DHTKey . BS.cons (typeByte type_)) <$> secureRandomBS 19

data DHTNode = DHTNode { dhtPeer   :: Peer
                       , dhtNodeId :: DHTKey
                       }

data DHTException = IDClash | NodeDown | AllPeersUnavailable
  deriving (Show, Typeable)

instance Exception DHTException

class Monad m => MonadDHT m where
  joinNetwork :: [Peer] -> m ()

  -- Peer discovery: query DHT for random key
  -- Processing request, node will discover few other nodes
  -- We return these newly discovered nodes among with already known
  -- (List of known nodes is updated as well)
  discoverPeers :: DHTNodeType -> m [DHTNode]

  getKnownPeers :: m [DHTNode]

  currentNodeKey :: m DHTKey

instance MonadDHT m => MonadDHT (ResponseT m) where
  discoverPeers = lift . discoverPeers
  getKnownPeers = lift getKnownPeers
  currentNodeKey = lift currentNodeKey
  joinNetwork = lift . joinNetwork
