{-# LANGUAGE TypeFamilies #-}

module Pos.DHT.Real.Types
       ( KademliaDHTInstance (..)
       , DHTHandle
       ) where

import           Universum              hiding (fromStrict, toStrict)

import           Control.Concurrent.STM (TVar)
import qualified Data.ByteString        as BS
import           Data.ByteString.Lazy   (fromStrict, toStrict)

import qualified Network.Kademlia       as K

import           Pos.Binary.Class       (Bi (..), decodeOrFail, encodeStrict)
import           Pos.DHT.Model.Types    (DHTData, DHTKey)
import           Pos.Util.TimeWarp      (NetworkAddress)

fromBSBinary :: Bi b => BS.ByteString -> Either String (b, BS.ByteString)
fromBSBinary bs =
    case decodeOrFail $ fromStrict bs of
        Left (_, _, errMsg)  -> Left errMsg
        Right (rest, _, res) -> Right (res, toStrict rest)

instance Bi DHTData => K.Serialize DHTData where
  toBS = encodeStrict
  fromBS = fromBSBinary

instance Bi DHTKey => K.Serialize DHTKey where
  toBS = encodeStrict
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

-- | Instance of node for /Kademlia DHT/ algorithm.
data KademliaDHTInstance = KademliaDHTInstance
    { kdiHandle          :: !DHTHandle
    , kdiKey             :: !DHTKey
    , kdiInitialPeers    :: ![NetworkAddress]
    , kdiExplicitInitial :: !Bool
    , kdiKnownPeersCache :: !(TVar [NetworkAddress])
    , kdiDumpPath        :: !FilePath
    }
