{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.NewDHT.Real.Types
       ( KademliaDHT (..)
       , KademliaDHTInstance (..)
       , KademliaDHTInstanceConfig (..)
       , DHTHandle
       ) where

import           Universum                 hiding (async, fromStrict, mapConcurrently,
                                            toStrict)

import           Control.Concurrent.STM    (TVar)
import           Control.Lens              (iso)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Morph       (MFunctor (hoist))
import           Control.Monad.Trans.Class (MonadTrans)
import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)

import qualified Data.Map.Strict           as M
import           Message.Message           (BinaryP)
import           Mockable                  (Channel (..), Fork (..), MFunctor' (hoist'),
                                            Mockable (liftMockable), SharedAtomicT,
                                            ThreadId, fork, killThread, myThreadId)
import           Mockable                  (MonadMockable, ThreadId)
import qualified Network.Kademlia          as K
import           Node                      (Listener (..), Node, NodeId)
import           Serokell.Util.Lens        (WrappedM (..))
import           System.Wlog               (CanLog, HasLoggerName)

import           Pos.Binary.Class          (Bi (..), decodeOrFail, encode)
import           Pos.NewDHT.Model.Types    (DHTData, DHTKey, DHTNode (..),
                                            DHTNodeType (..))

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

-- | Instance of part of config.
data KademliaDHTInstanceConfig = KademliaDHTInstanceConfig
    { kdcPort            :: !Word16
    , kdcKeyOrType       :: !(Either DHTKey DHTNodeType)
    , kdcInitialPeers    :: ![DHTNode]
    , kdcExplicitInitial :: !Bool
    }

-- | Node of /Kademlia DHT/ algorithm with access to 'KademliaDHTContext'.
newtype KademliaDHT m a = KademliaDHT
    { unKademliaDHT :: ReaderT KademliaDHTInstance m a
    } deriving (Functor, Applicative, Monad, MonadFail, MonadThrow, MonadCatch, MonadIO,
                MonadMask, CanLog, HasLoggerName)

type instance Mockable.ThreadId (KademliaDHT m) = Mockable.ThreadId m

instance ( Mockable d m
         , MFunctor' d (ReaderT KademliaDHTInstance m) m
         , MFunctor' d (KademliaDHT m) (ReaderT KademliaDHTInstance m)
         ) => Mockable d (KademliaDHT m) where
    liftMockable dmt = KademliaDHT $ liftMockable $ hoist' unKademliaDHT dmt

instance Monad m => WrappedM (KademliaDHT m) where
    type UnwrappedM (KademliaDHT m) = ReaderT KademliaDHTInstance m
    _WrappedM = iso unKademliaDHT KademliaDHT

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift
