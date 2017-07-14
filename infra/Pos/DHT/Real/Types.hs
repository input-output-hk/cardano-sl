{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TupleSections #-}

module Pos.DHT.Real.Types
       ( KademliaDHTInstance (..)
       , DHTHandle
       ) where

import           Universum              hiding (fromStrict, toStrict)

import           Control.Concurrent.STM (TVar)
import qualified Data.ByteString        as BS

import qualified Network.Kademlia       as K

import           Data.Bifunctor         (bimap)
import           Pos.Binary.Class       (Bi (..), serialize', decodeFull)
import           Pos.DHT.Model.Types    (DHTData, DHTKey)
import           Pos.Util.TimeWarp      (NetworkAddress)

-- CSL-1296: Should we worry about leftovers?
-- Previous Store-based implementation was taking the leftover
-- out of the serialisation, whereas in CBOR we deserialise everything
-- in a gulp.
instance Bi DHTData => K.Serialize DHTData where
  toBS   = serialize'
  fromBS = bimap toString (,BS.empty) . decodeFull

instance Bi DHTKey => K.Serialize DHTKey where
  toBS   = serialize'
  fromBS = bimap toString (,BS.empty) . decodeFull

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
