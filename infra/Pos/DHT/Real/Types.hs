{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.DHT.Real.Types
       ( KademliaDHT
       , asksKademliaDHT
       , KademliaDHTInstance (..)
       , KademliaDHTInstanceConfig (..)
       , DHTHandle
       , WithKademliaDHTInstance (..)
       ) where

import           Universum                 hiding (fromStrict, toStrict)

import           Control.Concurrent.STM    (TVar)
import qualified Control.Monad.Ether.Implicit as Ether
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)
import           Mockable                  (ChannelT, Counter, Distribution, Gauge, Gauge,
                                            Promise, SharedAtomicT, SharedExclusiveT,
                                            SharedExclusiveT, ThreadId)
import qualified Network.Kademlia          as K

import           Pos.Binary.Class          (Bi (..), decodeOrFail, encode)
import           Pos.DHT.Model.Types       (DHTData, DHTKey, DHTNode (..))

toBSBinary :: Bi b => b -> BS.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Bi b => BS.ByteString -> Either String (b, BS.ByteString)
fromBSBinary bs =
    case decodeOrFail $ fromStrict bs of
        Left (_, _, errMsg)  -> Left errMsg
        Right (rest, _, res) -> Right (res, toStrict rest)

instance Bi DHTData => K.Serialize DHTData where
  toBS = toBSBinary
  fromBS = fromBSBinary

instance Bi DHTKey => K.Serialize DHTKey where
  toBS = toBSBinary
  fromBS = fromBSBinary

type DHTHandle = K.KademliaInstance DHTKey DHTData

-- | Instance of node for /Kademlia DHT/ algorithm.
data KademliaDHTInstance = KademliaDHTInstance
    { kdiHandle          :: !DHTHandle
    , kdiKey             :: !DHTKey
    , kdiInitialPeers    :: ![DHTNode]
    , kdiExplicitInitial :: !Bool
    , kdiKnownPeersCache :: !(TVar [K.Node DHTKey])
    }

-- | Instance of part of config.
data KademliaDHTInstanceConfig = KademliaDHTInstanceConfig
    { kdcHost            :: !BS.ByteString
    , kdcPort            :: !Word16
    , kdcKey             :: !(Maybe DHTKey)
    , kdcInitialPeers    :: ![DHTNode]
    , kdcExplicitInitial :: !Bool
    , kdcDumpPath        :: !FilePath
    } deriving (Show)

-- | Node of /Kademlia DHT/ algorithm with access to 'KademliaDHTContext'.
type KademliaDHT = Ether.ReaderT KademliaDHTInstance

-- | Class for getting KademliaDHTInstance from 'KademliaDHT'
class Monad m => WithKademliaDHTInstance m where
    getKademliaDHTInstance :: m KademliaDHTInstance

    default getKademliaDHTInstance
      :: (WithKademliaDHTInstance m', MonadTrans t, m ~ t m')
      => m KademliaDHTInstance
    getKademliaDHTInstance = lift getKademliaDHTInstance

asksKademliaDHT
  :: WithKademliaDHTInstance m
  => (KademliaDHTInstance -> a)
  -> m a
asksKademliaDHT g = g <$> getKademliaDHTInstance

instance {-# OVERLAPPABLE #-}
  (WithKademliaDHTInstance m, MonadTrans t, Monad (t m)) =>
  WithKademliaDHTInstance (t m)

instance Monad m => WithKademliaDHTInstance (KademliaDHT m) where
    getKademliaDHTInstance = Ether.ask

type instance ThreadId (KademliaDHT m) = ThreadId m
type instance Promise (KademliaDHT m) = Promise m
type instance SharedAtomicT (KademliaDHT m) = SharedAtomicT m
type instance Counter (KademliaDHT m) = Counter m
type instance Distribution (KademliaDHT m) = Distribution m
type instance SharedExclusiveT (KademliaDHT m) = SharedExclusiveT m
type instance Gauge (KademliaDHT m) = Gauge m
type instance ChannelT (KademliaDHT m) = ChannelT m
