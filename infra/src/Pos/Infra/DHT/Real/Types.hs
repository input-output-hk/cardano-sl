{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies  #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Infra.DHT.Real.Types
       ( KademliaDHTInstance (..)
       , DHTHandle
       ) where

import           Universum

import           Control.Concurrent.STM (TVar)

import qualified Network.Kademlia as K

import           Data.Bifunctor (bimap)
import           Pos.Binary.Class (deserializeOrFail', serialize')
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.DHT.Model.Types (DHTData, DHTKey)


instance K.Serialize DHTData where
    toBS   = serialize'
    fromBS = bimap (show . fst) identity . deserializeOrFail'

instance K.Serialize DHTKey where
    toBS   = serialize'
    fromBS = bimap (show . fst) identity . deserializeOrFail'

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
