{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.NewDHT.Real.Types
       ( KademliaDHTInstance (..)
       , KademliaDHTContext (..)
       , KademliaDHTConfig (..)
       , KademliaDHTInstanceConfig (..)
       , KademliaDHT (..)
       , DHTHandle
       ) where

import           Universum                 hiding (async, fromStrict, mapConcurrently,
                                            toStrict)

import           Control.Concurrent.STM    (TVar)
import           Control.Lens              (iso)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Morph       (MFunctor (hoist))
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Rpc      (Binding (..), MonadDialog, MonadResponse (..),
                                            MonadTransfer (..), hoistRespCond)
import           Control.TimeWarp.Timed    (ThreadId)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)

import           Message.Message           (BinaryP)
import           Mockable.Channel          (Channel (..))
import           Mockable.Class            (MFunctor' (hoist'), Mockable (liftMockable))
import           Mockable.Concurrent       (Fork (..), ThreadId, fork, killThread,
                                            myThreadId)
import           Mockable.Monad            (MonadMockable)
import qualified Network.Kademlia          as K
import           Node                      (Listener (..))
import           Pos.Binary.Class          (Bi (..), decodeOrFail, encode)
import           Pos.NewDHT.Model.Types    (DHTData, DHTKey, DHTNode (..),
                                            DHTNodeType (..))
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName)

toBSBinary :: Bi b => b -> BS.ByteString
toBSBinary = toStrict . encode

fromBSBinary :: Bi b => BS.ByteString -> Either [Char] (b, BS.ByteString)
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

-- | Node context for 'KademliaDHTInstance'.
data KademliaDHTContext m = KademliaDHTContext
    { kdcDHTInstance_         :: !KademliaDHTInstance
    , kdcAuxClosers           :: !(TVar [KademliaDHT m ()])
    , kdcListenByBinding      :: !(Binding -> KademliaDHT m (KademliaDHT m ()))
    , kdcStopped              :: !(TVar Bool)
    , kdcNoCacheMessageNames_ :: ![Text]
    }

-- | Configuration for particular 'KademliaDHTInstance'.
data KademliaDHTConfig s m = KademliaDHTConfig
    { kdcPort                :: !Word16
    , kdcListeners           :: ![Listener BinaryP s m]
    , kdcMessageCacheSize    :: !Int
    , kdcEnableBroadcast     :: !Bool
    , kdcNoCacheMessageNames :: ![Text]
    , kdcDHTInstance         :: !KademliaDHTInstance
    }

-- | Instance of part of config.
data KademliaDHTInstanceConfig = KademliaDHTInstanceConfig
    { kdcPort            :: !Word16
    , kdcKeyOrType       :: !(Either DHTKey DHTNodeType)
    , kdcInitialPeers    :: ![DHTNode]
    , kdcExplicitInitial :: !Bool
    }

-- | Node of /Kademlia DHT/ algorithm with access to 'KademliaDHTContext'.
newtype KademliaDHT m a = KademliaDHT
    { unKademliaDHT :: ReaderT (KademliaDHTContext m) m a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadThrow, MonadCatch, MonadIO,
                MonadMask, MonadDialog s p, CanLog, HasLoggerName)

type instance Mockable.Concurrent.ThreadId (KademliaDHT m) = Mockable.Concurrent.ThreadId m

instance ( Mockable d m
         , MFunctor' d (ReaderT (KademliaDHTContext m) m) m
         , MFunctor' d (KademliaDHT m) (ReaderT (KademliaDHTContext m) m)
         ) => Mockable d (KademliaDHT m) where
    liftMockable dmt = KademliaDHT $ liftMockable $ hoist' unKademliaDHT dmt

instance Monad m => WrappedM (KademliaDHT m) where
    type UnwrappedM (KademliaDHT m) = ReaderT (KademliaDHTContext m) m
    _WrappedM = iso unKademliaDHT KademliaDHT

instance MonadResponse s m => MonadResponse s (KademliaDHT m) where
    replyRaw dat = KademliaDHT $ replyRaw (hoist unKademliaDHT dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

instance MonadTransfer s m => MonadTransfer s (KademliaDHT m) where
    sendRaw addr req = KademliaDHT $ sendRaw addr (hoist unKademliaDHT req)
    listenRaw binding sink =
        KademliaDHT $ fmap KademliaDHT $ listenRaw binding $ hoistRespCond unKademliaDHT sink
    close = lift . close
    userState = lift . userState

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift
