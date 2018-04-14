-- | Provides functionality of representing `Bi` instances as correct
-- `Message`s used by time-warp.

module Pos.Binary.Infra.DHTModel () where

import           Universum

import           Network.Kademlia as K
import           Network.Kademlia.HashNodeId (HashId (..))
import           Network.Kademlia.Instance (BanState)
import           Network.Kademlia.Tree as K
import           Network.Kademlia.Types as K

import           Pos.Binary.Class (Bi (..), encodeListLen, enforceSize, genericDecode,
                                   genericEncode, genericEncodedSize)
import           Pos.DHT.Model.Types (DHTData (..), DHTKey (..))

instance Bi DHTKey where
    encode (DHTKey (HashId bs)) = encode bs
    encodedSize (DHTKey (HashId bs)) = encodedSize bs
    decode = DHTKey . HashId <$> decode

instance Bi DHTData where
    encode (DHTData unit) = encode unit
    encodedSize (DHTData unit) = encodedSize unit
    decode = DHTData <$> decode

-- CSL-1296: Orphan (inefficient) Kademlia instances.

instance Bi PingInfo where
    encode = genericEncode
    encodedSize = genericEncodedSize
    decode = genericDecode

instance Bi i => Bi (K.Node i) where
    encode = genericEncode
    encodedSize = genericEncodedSize
    decode = genericDecode

instance Bi BanState where
    encode = genericEncode
    encodedSize = genericEncodedSize
    decode = genericDecode

instance Bi K.Peer where
    encode p =
        encodeListLen 2 <> encode (K.peerHost p) <> encode (K.unwrapPort . K.peerPort $ p)
    encodedSize p =
        1 + encodedSize (K.peerHost p)
          + encodedSize (K.unwrapPort . K.peerPort $ p)
    decode   = do
        enforceSize "Kademlia.Peer" 2
        K.Peer <$> decode <*> (K.wrapPort <$> decode)

instance Bi i => Bi (K.NodeTreeElem i) where
    encode = genericEncode
    encodedSize = genericEncodedSize
    decode = genericDecode

instance Bi i => Bi (K.NodeTree i) where
    encode = genericEncode
    encodedSize = genericEncodedSize
    decode = genericDecode

instance Bi i => Bi (K.KademliaSnapshot i) where
    encode = genericEncode
    encodedSize = genericEncodedSize
    decode = genericDecode
