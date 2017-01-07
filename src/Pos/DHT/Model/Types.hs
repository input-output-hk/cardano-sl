-- | DHT types.

module Pos.DHT.Model.Types
       ( DHTData (..)
       , DHTKey (..)
       , DHTNode (..)
       , bytesToDHTKey
       , randomDHTKey
       ) where

import           Control.TimeWarp.Rpc        (NetworkAddress)
import qualified Data.ByteString             as BS
import           Data.Hashable               (Hashable (..))
import           Data.Text.Buildable         (Buildable (..))
import           Formatting                  (bprint, (%))
import qualified Formatting                  as F
import           Network.Kademlia            (fromBS)
import           Network.Kademlia.HashNodeId (HashId (..), genNonce, hashAddress)
import           Prelude                     (show)
import           Serokell.Util.Text          (listBuilderJSON)
import           Universum                   hiding (show)

import           Pos.Crypto.Random           (runSecureRandom)

-- | Dummy data for DHT.
newtype DHTData = DHTData ()
  deriving (Eq, Ord, Show, Generic)

-- | DHTKey should be strictly 20-byte long.
newtype DHTKey = DHTKey { hashNodeId :: HashId }
  deriving (Eq, Ord, Generic)

instance Hashable DHTKey where
    hashWithSalt s (DHTKey (HashId bs)) = hashWithSalt s bs

instance Buildable DHTKey where
    build = build . show

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
randomDHTKey = liftIO $ DHTKey . hashAddress <$> runSecureRandom genNonce
