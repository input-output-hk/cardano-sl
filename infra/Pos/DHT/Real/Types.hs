{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.DHT.Real.Types
       ( KademliaDHT (..)
       , KademliaDHTInstance (..)
       , KademliaDHTInstanceConfig (..)
       , DHTHandle
       , WithKademliaDHTInstance (..)
       ) where

import           Universum                 hiding (fromStrict, toStrict)

import           Control.Concurrent.STM    (TVar)
import           Control.Lens              (iso)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Fix         (MonadFix)
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)

import           Mockable                  (ChannelT, Counter, Distribution, Gauge, Gauge,
                                            MFunctor', Mockable (liftMockable), Promise,
                                            SharedAtomicT, SharedExclusiveT,
                                            SharedExclusiveT, ThreadId,
                                            liftMockableWrappedM)
import qualified Network.Kademlia          as K
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName)

import           Pos.Binary.Class          (Bi (..), decodeOrFail, encode)
import           Pos.DHT.Model.Types       (DHTData, DHTKey, DHTNode (..))
import           Pos.Util.Context          (MonadContext (..))

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
    }

-- | Node of /Kademlia DHT/ algorithm with access to 'KademliaDHTContext'.
newtype KademliaDHT m a = KademliaDHT
    { unKademliaDHT :: ReaderT KademliaDHTInstance m a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadThrow, MonadCatch, MonadIO,
                MonadMask, CanLog, HasLoggerName, MonadTrans, MonadFix)

instance MonadContext m => MonadContext (KademliaDHT m) where
    type ContextType (KademliaDHT m) = ContextType m

-- | Class for getting KademliaDHTInstance from 'KademliaDHT'
class WithKademliaDHTInstance m where
    getKademliaDHTInstance :: m KademliaDHTInstance

instance Monad m => WithKademliaDHTInstance (KademliaDHT m) where
    getKademliaDHTInstance = KademliaDHT ask

instance (Monad m, WithKademliaDHTInstance m) =>
         WithKademliaDHTInstance (ReaderT a m) where
    getKademliaDHTInstance = lift getKademliaDHTInstance

instance (Monad m, WithKademliaDHTInstance m) =>
         WithKademliaDHTInstance (StateT a m) where
    getKademliaDHTInstance = lift getKademliaDHTInstance

type instance ThreadId (KademliaDHT m) = ThreadId m
type instance Promise (KademliaDHT m) = Promise m
type instance SharedAtomicT (KademliaDHT m) = SharedAtomicT m
type instance Counter (KademliaDHT m) = Counter m
type instance Distribution (KademliaDHT m) = Distribution m
type instance SharedExclusiveT (KademliaDHT m) = SharedExclusiveT m
type instance Gauge (KademliaDHT m) = Gauge m
type instance ChannelT (KademliaDHT m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT KademliaDHTInstance m) m
         , MFunctor' d (KademliaDHT m) (ReaderT KademliaDHTInstance m)
         ) => Mockable d (KademliaDHT m) where
    liftMockable = liftMockableWrappedM

instance Monad m => WrappedM (KademliaDHT m) where
    type UnwrappedM (KademliaDHT m) = ReaderT KademliaDHTInstance m
    _WrappedM = iso unKademliaDHT KademliaDHT
