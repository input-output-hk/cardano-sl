{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

module Pos.DHT.Real.Types
       ( KademliaDHTInstance (..)
       , DHTHandle
       ) where

import           Universum              hiding (fromStrict, toStrict)

import           Control.Concurrent.STM (TVar)
import qualified Network.Kademlia       as K

import           Pos.Binary.Class       (Bi (..), decodeFull, serialize')
import           Pos.DHT.Model.Types    (DHTData, DHTKey)
import           Pos.Util.TimeWarp      (NetworkAddress)


instance Bi DHTData => K.Serialize DHTData where
    toBS   = serialize'
    fromBS = first toString . decodeFull

instance Bi DHTKey => K.Serialize DHTKey where
    toBS   = serialize'
    fromBS = first toString . decodeFull

type DHTHandle = K.KademliaInstance DHTKey DHTData

-- | Instance of node for /Kademlia DHT/ algorithm.
data KademliaDHTInstance = KademliaDHTInstance
    { kdiHandle          :: !DHTHandle
    , kdiKey             :: !DHTKey
    , kdiInitialPeers    :: ![NetworkAddress]
    , kdiExplicitInitial :: !Bool
    , kdiKnownPeersCache :: !(TVar [NetworkAddress])
    , kdiDumpPath        :: !(Maybe FilePath)
    }
