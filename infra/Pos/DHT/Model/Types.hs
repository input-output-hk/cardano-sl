-- | DHT types.

module Pos.DHT.Model.Types
       ( DHTData (..)
       , DHTKey (..)
       , DHTNode (..)
       , DHTException (..)
       , bytesToDHTKey
       , randomDHTKey
       , getMeaningPart
       , meaningPartLength

       -- * Parsers
       , dhtKeyParser
       , dhtNodeParser

       , dhtNodeToNodeId
       ) where

import qualified Control.Monad               as Monad (fail)
import qualified Data.ByteString             as BS
import           Data.Hashable               (Hashable (..))
import           Data.Text.Buildable         (Buildable (..))
import           Formatting                  (bprint, (%))
import qualified Formatting                  as F
import           Network.Kademlia            (fromBS)
import           Network.Kademlia.HashNodeId (HashId (..), genNonce, hashAddress)
import qualified Prelude
import qualified Serokell.Util.Base64        as B64
import qualified Serokell.Util.Parse         as P
import           Serokell.Util.Text          (listBuilderJSON)
import qualified Text.Parsec.Char            as P
import qualified Text.Parsec.String          as P
import           Universum

import           Pos.Communication.Protocol  (NodeId)
import           Pos.Crypto.Random           (runSecureRandom)
import           Pos.Util.TimeWarp           (NetworkAddress, addrParser, addressToNodeId)

-- | Data type for DHT exceptions.
data DHTException = NodeDown | AllPeersUnavailable
  deriving (Show, Typeable)

instance Exception DHTException

-- TODO export lengths from HashNodeId module
meaningPartLength :: Int
meaningPartLength = 14

hashPartLength :: Int
hashPartLength = 18

getMeaningPart :: DHTKey -> ByteString
getMeaningPart (DHTKey (HashId bs)) = snd $ BS.splitAt hashPartLength bs

-- | Dummy data for DHT.
newtype DHTData = DHTData ()
  deriving (Eq, Ord, Show, Generic)

-- | DHT key, 32-byte long (18-byte hash + 14-byte nonce)
newtype DHTKey = DHTKey { hashNodeId :: HashId }
  deriving (Eq, Ord, Generic)

instance Hashable DHTKey where
    hashWithSalt s (DHTKey (HashId bs)) = hashWithSalt s bs

instance Buildable DHTKey where
    build (DHTKey (HashId bs)) = build (B64.encodeUrl bs)

instance Show DHTKey where
    show = toString . pretty

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

----------------------------------------------------------------------------
-- Parsers
----------------------------------------------------------------------------

-- | Parser for DHT key.
dhtKeyParser :: P.Parser DHTKey
dhtKeyParser = P.base64Url >>= toDHTKey
  where
    toDHTKey = either Monad.fail return . bytesToDHTKey

-- | Parser for 'DHTNode'.
dhtNodeParser :: P.Parser DHTNode
dhtNodeParser = DHTNode <$> addrParser <*> (P.char '/' *> dhtKeyParser)

-- | FIXME this is flimsy. It assumes that the node has a TCP server running
--   at the same host/port of its Kademlia instance, and that its 0'th EndPoint
--   is the desired target.
--
--   TBD would storing the opaque NodeId (a ByteString) as a value in the DHT,
--   keyed on the Kademlia ID of its host, work well?
dhtNodeToNodeId :: DHTNode -> (DHTKey, NodeId)
dhtNodeToNodeId dhtNode =
    let twNodeId = addressToNodeId . dhtAddr $ dhtNode
    in  (dhtNodeId dhtNode, twNodeId)
