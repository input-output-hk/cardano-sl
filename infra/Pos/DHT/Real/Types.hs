{-# LANGUAGE TypeFamilies #-}

module Pos.DHT.Real.Types
       ( KademliaDHTInstance (..)
       , DHTHandle
       ) where

import           Universum              hiding (fromStrict, toStrict)

import           Control.Concurrent.STM (TVar)
import qualified Data.ByteString        as BS

import           Data.Store             (PeekException (..), decodeIOPortionWith)
import qualified Network.Kademlia       as K

import           Pos.Binary.Class       (Bi (..), encode)
import           Pos.DHT.Model.Types    (DHTData, DHTKey)
import           Pos.Network.Types      (NodeType)
import           Pos.Util.TimeWarp      (NetworkAddress)
import           System.IO.Unsafe       (unsafePerformIO)

fromBSBinary :: Bi b => BS.ByteString -> Either String (b, BS.ByteString)
fromBSBinary bs = unsafePerformIO $
    (decodeIOPortionWith get bs >>= \(off, res) -> return $ Right (res, BS.drop off bs))
      `catch` handler
  where
    handler (PeekException {..}) = return $ Left (toString peekExMessage)

instance Bi DHTData => K.Serialize DHTData where
  toBS = encode
  fromBS = fromBSBinary

instance Bi DHTKey => K.Serialize DHTKey where
  toBS = encode
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

-- | Instance of node for /Kademlia DHT/ algorithm.
data KademliaDHTInstance = KademliaDHTInstance
    { kdiHandle          :: !DHTHandle
    , kdiKey             :: !DHTKey
    , kdiInitialPeers    :: ![NetworkAddress]
    , kdiExplicitInitial :: !Bool
    , kdiKnownPeersCache :: !(TVar [NetworkAddress])
    , kdiDumpPath        :: !(Maybe FilePath)
    , kdiPeerType        :: !NodeType
    }
