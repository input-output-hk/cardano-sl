-- | DHT types.

module Pos.NewDHT.Model.Types
       ( DHTData (..)
       , DHTKey (..)
       , DHTNode (..)
       , DHTNodeType (..)
       , bytesToDHTKey
       , dhtNodeType
       , randomDHTKey
       , typeByte
       , filterByNodeType
       , addressToNodeId
       , nodeIdToAddress
       ) where

import           Control.TimeWarp.Rpc  (NetworkAddress)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import           Data.Char             (isNumber)
import           Data.Hashable         (Hashable)
import           Data.Text.Buildable   (Buildable (..))
import           Formatting            (bprint, (%))
import qualified Formatting            as F
import qualified Network.Transport.TCP as TCP
import           Node                  (NodeId (..))
import           Prelude               (read, show)
import qualified Serokell.Util.Base64  as B64
import           Serokell.Util.Text    (listBuilderJSON)
import           Universum             hiding (show)


import           Pos.Crypto.Random     (secureRandomBS)

-- | Dummy data for DHT.
newtype DHTData = DHTData ()
  deriving (Eq, Ord, Show, Generic)

-- | DHTKey should be strictly 20-byte long.
newtype DHTKey = DHTKey { dhtKeyBytes :: BS.ByteString }
  deriving (Eq, Ord, Hashable, Generic)

instance Buildable DHTKey where
    build key@(DHTKey bs) = buildType (dhtNodeType key)
                `mappend` build ' '
                `mappend` build (B64.encodeUrl bs)
      where
        buildType Nothing  = "<Unknown type>"
        buildType (Just s) = build s

instance Show DHTKey where
  show = toString . pretty

-- | Node type is determined by first byte of key.
data DHTNodeType
    -- | Node which participates only in supporting DHT, i.e. not a part of PoS communication.
    = DHTSupporter
    -- | Full node, i.e. fully participating in both DHT supporting and PoS.
    | DHTFull
    -- | Client node (for SPV). Key idea is that clients, being a part of DHT, are rarely queried.
    | DHTClient
    deriving (Eq, Ord, Show)

instance Buildable DHTNodeType where
    build = build . show

-- | Return type of DHT node by given key.
dhtNodeType :: DHTKey -> Maybe DHTNodeType
dhtNodeType (DHTKey bs) = impl $ BS.head bs
  where
    impl 0x00 = Just DHTSupporter
    impl 0x30 = Just DHTFull
    impl 0xF0 = Just DHTClient
    impl _    = Nothing

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
bytesToDHTKey bs =
    if BS.length bs /= 20
    then Left "Key length must be exactly 20 bytes"
    else Right $ DHTKey bs

-- | Generate random 'DHTKey'.
randomDHTKey :: MonadIO m => DHTNodeType -> m DHTKey
randomDHTKey type_ = (DHTKey . BS.cons (typeByte type_)) <$> secureRandomBS 19

-- | Get byte representation of 'DHTNodeType'.
typeByte :: DHTNodeType -> Word8
typeByte DHTSupporter = 0x00
typeByte DHTFull      = 0x30
typeByte DHTClient    = 0xF0

-- | Leave only those nodes that has given @Just type@.
filterByNodeType :: DHTNodeType -> [DHTNode] -> [DHTNode]
filterByNodeType type_ = filter (\n -> dhtNodeType (dhtNodeId n) == Just type_)

-- TODO: What about node index, i.e. last number in '127.0.0.1:3000:0' ?
addressToNodeId :: NetworkAddress -> NodeId
addressToNodeId (host, port) = NodeId $ TCP.encodeEndPointAddress (BS8.unpack host) (show port) 0

nodeIdToAddress :: NodeId -> Maybe NetworkAddress
nodeIdToAddress (NodeId ep) = toNA =<< TCP.decodeEndPointAddress ep
  where
    toNA (hostName, port', _) = (BS8.pack hostName,) <$> toPort port'
    toPort :: [Char] -> Maybe Word16
    toPort port' | all isNumber port' = pure $ read port'
                 | otherwise          = Nothing

