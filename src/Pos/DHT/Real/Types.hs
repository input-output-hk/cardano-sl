{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Pos.DHT.Real.Types
       ( KademliaDHTInstance (..)
       , KademliaDHTContext (..)
       , KademliaDHTConfig (..)
       , KademliaDHTInstanceConfig (..)
       , KademliaDHT (..)
       , DHTHandle
       ) where

import           Control.Concurrent.STM    (TVar)
import           Control.Monad.Catch       (MonadCatch, MonadMask, MonadThrow)
import           Control.Monad.Morph       (hoist)
import           Control.Monad.Trans.Class (MonadTrans)
import           Control.TimeWarp.Rpc      (Binding (..), MonadDialog, MonadResponse (..),
                                            MonadTransfer (..), hoistRespCond)
import           Control.TimeWarp.Timed    (MonadTimed, ThreadId)

import qualified Data.ByteString           as BS
import           Data.ByteString.Lazy      (fromStrict, toStrict)

import qualified Network.Kademlia          as K
import           Pos.Binary.Class          (Bi (..), decodeOrFail, encode)
import           System.Wlog               (CanLog, HasLoggerName)
import           Universum                 hiding (async, fromStrict, mapConcurrently,
                                            toStrict)

import           Pos.DHT.Model.Class       (DHTMsgHeader (..), ListenerDHT (..),
                                            WithDefaultMsgHeader (..))
import           Pos.DHT.Model.Types       (DHTData, DHTKey, DHTNode (..),
                                            DHTNodeType (..))
#ifdef WITH_ROCKS
import           Pos.Modern.Txp.Class      (MonadTxpLD (..))
#endif

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
    , kdcListeners           :: ![ListenerDHT s (KademliaDHT m)]
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
newtype KademliaDHT m a = KademliaDHT { unKademliaDHT :: ReaderT (KademliaDHTContext m) m a }
    deriving (Functor, Applicative, Monad, MonadThrow, MonadCatch, MonadIO,
             MonadMask, MonadTimed, MonadDialog s p, CanLog, HasLoggerName
#ifdef WITH_ROCKS
                , MonadTxpLD ssc
#endif
                )

instance MonadResponse s m => MonadResponse s (KademliaDHT m) where
    replyRaw dat = KademliaDHT $ replyRaw (hoist unKademliaDHT dat)
    closeR = lift closeR
    peerAddr = lift peerAddr

--instance MonadTransControl KademliaDHT where
--    type StT KademliaDHT a = StT (ReaderT (KademliaDHTContext m)) a
--    liftWith = defaultLiftWith KademliaDHT unKademliaDHT
--    restoreT = defaultRestoreT KademliaDHT
--
--instance MonadBaseControl IO m => MonadBaseControl IO (KademliaDHT m) where
--    type StM (KademliaDHT m) a = ComposeSt KademliaDHT m a
--    liftBaseWith     = defaultLiftBaseWith
--    restoreM         = defaultRestoreM

instance MonadTransfer s m => MonadTransfer s (KademliaDHT m) where
    sendRaw addr req = KademliaDHT $ sendRaw addr (hoist unKademliaDHT req)
    listenRaw binding sink =
        KademliaDHT $ fmap KademliaDHT $ listenRaw binding $ hoistRespCond unKademliaDHT sink
    close = lift . close
    userState = lift . userState

instance Applicative m => WithDefaultMsgHeader (KademliaDHT m) where
    defaultMsgHeader _ = do
        --     Caches are disabled now for non-broadcast messages
        --     uncomment lines below to enable them
        --noCacheNames <- KademliaDHT $ asks kdcNoCacheMessageNames_
        --let header =
        --        SimpleHeader . isJust . find (== messageName' msg) $
        --        noCacheNames
        let header = SimpleHeader True
        --withDhtLogger $
        --    logDebug $
        --    sformat
        --        ("Preparing message " % stext % ": header " % shown)
        --        (messageName' msg)
        --        header
        pure header

instance MonadTrans KademliaDHT where
  lift = KademliaDHT . lift

type instance ThreadId (KademliaDHT m) = ThreadId m
