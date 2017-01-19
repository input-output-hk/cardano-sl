{-# LANGUAGE TemplateHaskell #-}

-- | DHT types.

module Pos.DHT.Model.Types
       ( DHTData (..)
       , DHTKey (..)
       , DHTNode (..)
       , bytesToDHTKey
       , randomDHTKey
       , addressToNodeId
       , addressToNodeId'
       , nodeIdToAddress
       ) where

import qualified Data.ByteString             as BS
import qualified Data.ByteString.Char8       as BS8
import           Data.Char                   (isNumber)
import           Data.Hashable               (Hashable)
import           Data.Hashable               (Hashable (..))
import           Data.Text.Buildable         (Buildable (..))
import           Formatting                  (bprint, (%))
import qualified Formatting                  as F
import           Network.Kademlia            (fromBS)
import           Network.Kademlia.HashNodeId (HashId (..), genNonce, hashAddress)
import qualified Network.Transport.TCP       as TCP
import           Node                        (NodeId (..))
import           Prelude                     (read, show)
import qualified Serokell.Util.Base64        as B64
import           Serokell.Util.Text          (listBuilderJSON)
import           Universum                   hiding (show)

import           Pos.Crypto.Random           (runSecureRandom)
import           Pos.Util.TimeWarp           (NetworkAddress)
import           Test.QuickCheck             (Arbitrary (..))

-- | Dummy data for DHT.
newtype DHTData = DHTData ()
  deriving (Eq, Ord, Show, Generic)

-- | DHTKey should be strictly 20-byte long.
newtype DHTKey = DHTKey { hashNodeId :: HashId }
  deriving (Eq, Ord, Generic)

instance Hashable DHTKey where
    hashWithSalt s (DHTKey (HashId bs)) = hashWithSalt s bs

instance Buildable DHTKey where
    build (DHTKey (HashId bs)) = build (B64.encodeUrl bs)

instance Show DHTKey where
    show = toString . pretty

deriving instance Arbitrary DHTData

instance Arbitrary DHTKey where
    arbitrary = DHTKey . HashId . BS.pack <$> arbitrary

-- | DHT node.
data DHTNode = DHTNode { dhtAddr   :: NetworkAddress
                       , dhtNodeId :: DHTKey
                       }
  deriving (Eq, Ord, Show)

instance Buildable DHTNode where
    build (DHTNode (peerHost, peerPort) key)
      = bprint (F.build % " at " % F.stext % ":" % F.build)
               key
               (decodeUtf8 peerHost)
               peerPort

instance Buildable [DHTNode] where
    build = listBuilderJSON

-- | Converts 'BS.ByteString' into 'DHTKey' if possible.
bytesToDHTKey :: IsString s => BS.ByteString -> Either s DHTKey
bytesToDHTKey bs = either (Left . fromString) (Right . DHTKey . fst) $ fromBS bs

-- | Generate random 'DHTKey'.
randomDHTKey :: MonadIO m => m DHTKey
randomDHTKey = DHTKey . hashAddress <$> liftIO (runSecureRandom genNonce)

-- TODO: What about node index, i.e. last number in '127.0.0.1:3000:0' ?
addressToNodeId :: NetworkAddress -> NodeId
addressToNodeId = addressToNodeId' 0

addressToNodeId' :: Word32 -> NetworkAddress -> NodeId
addressToNodeId' eId (host, port) = NodeId $ TCP.encodeEndPointAddress (BS8.unpack host) (show port) eId

nodeIdToAddress :: NodeId -> Maybe NetworkAddress
nodeIdToAddress (NodeId ep) = toNA =<< TCP.decodeEndPointAddress ep
  where
    toNA (hostName, port', _) = (BS8.pack hostName,) <$> toPort port'
    toPort :: [Char] -> Maybe Word16
    toPort port' | all isNumber port' = pure $ read port'
                 | otherwise          = Nothing
