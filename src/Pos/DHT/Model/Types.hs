{-# LANGUAGE TemplateHaskell #-}

-- | DHT types.

module Pos.DHT.Model.Types
       ( DHTData (..)
       , DHTKey (..)
       , DHTNode (..)
       , bytesToDHTKey
       , randomDHTKey
       , getMeaningPart
       , meaningPartLength
       ) where

import qualified Data.ByteString             as BS
import           Data.Hashable               (Hashable)
import           Data.Hashable               (Hashable (..))
import           Data.Text.Buildable         (Buildable (..))
import           Formatting                  (bprint, (%))
import qualified Formatting                  as F
import           Network.Kademlia            (fromBS)
import           Network.Kademlia.HashNodeId (HashId (..), genNonce, hashAddress)
import qualified Prelude                     as Prelude
import qualified Serokell.Util.Base64        as B64
import           Serokell.Util.Text          (listBuilderJSON)
import           Universum

import           Pos.Crypto.Random           (runSecureRandom)
import           Pos.Util.TimeWarp           (NetworkAddress)
import           Test.QuickCheck             (Arbitrary (..))

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
