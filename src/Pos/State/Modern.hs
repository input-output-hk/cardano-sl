{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

-- | Modern implementation of node's persistent state.

module Pos.State.Modern
       ( NodeDB
       , MonadDB (..)
       , openNodeDB
       , runNodeDBHolder

       , Patak (..)
       , getPatak
       , putPatak
       ) where

import           Universum

import           Control.Monad.Base           (MonadBase (..))
import           Control.Monad.Trans          (MonadTrans)
import           Control.Monad.Trans.Control  (ComposeSt, MonadBaseControl (..),
                                               MonadTransControl (..), StM,
                                               defaultLiftBaseWith, defaultLiftWith,
                                               defaultRestoreM, defaultRestoreT)
import           Control.Monad.Trans.Resource (MonadResource, MonadResourceBase,
                                               ResourceT, runResourceT)
import           Control.TimeWarp.Timed       (MonadTimed, ThreadId)
import           Data.Default                 (def)
import qualified Database.RocksDB             as Rocks
import           System.Wlog                  (CanLog, HasLoggerName)

class MonadIO m => MonadDB m where
    getNodeDB :: m (NodeDB ())

newtype NodeDB ssc = NodeDB Rocks.DB

openNodeDB :: MonadResource m => FilePath -> m (NodeDB ssc)
openNodeDB fp = NodeDB <$> Rocks.open fp def { Rocks.createIfMissing = True }

newtype NodeDBHolder m a = NodeDBHolder
    { getNodeDBHolder :: ReaderT (NodeDB ()) m a
    } deriving (Functor, Applicative, Monad, MonadTrans, MonadTimed, MonadThrow,
               MonadCatch, MonadMask, MonadIO, HasLoggerName, CanLog)

instance MonadBase IO m => MonadBase IO (NodeDBHolder m) where
    liftBase = lift . liftBase

deriving instance MonadResource m => MonadResource (NodeDBHolder m)

type instance ThreadId (NodeDBHolder m) = ThreadId m

instance MonadIO m =>
         MonadDB (NodeDBHolder m) where
    getNodeDB = NodeDBHolder ask

instance MonadTransControl NodeDBHolder where
    type StT NodeDBHolder a = StT (ReaderT (NodeDB ())) a
    liftWith = defaultLiftWith NodeDBHolder getNodeDBHolder
    restoreT = defaultRestoreT NodeDBHolder

instance MonadBaseControl IO m => MonadBaseControl IO (NodeDBHolder m) where
    type StM (NodeDBHolder m) a = ComposeSt NodeDBHolder m a
    liftBaseWith     = defaultLiftBaseWith
    restoreM         = defaultRestoreM

-- | Execute 'DBHolder' action with given 'NodeState'.
runNodeDBHolder :: NodeDB () -> NodeDBHolder m a -> m a
runNodeDBHolder db = flip runReaderT db . getNodeDBHolder

newtype Patak =
    Patak ByteString
    deriving (Show)

patakKey :: ByteString
patakKey = "patak"

getPatak :: MonadDB m => m (Maybe Patak)
getPatak = fmap Patak <$> myGet patakKey

putPatak :: MonadDB m => Patak -> m ()
putPatak (Patak p) = myPut patakKey p

myGet :: MonadDB m => ByteString -> m (Maybe ByteString)
myGet k = do
    NodeDB db <- getNodeDB
    liftIO $ runResourceT $ Rocks.get db def k

myPut :: MonadDB m => ByteString -> ByteString -> m ()
myPut k v = do
    NodeDB db <- getNodeDB
    liftIO $ runResourceT $ Rocks.put db def k v

-- Garbage

-- type TmpWorkMode ssc m = (MonadResourceBase m, MonadDB ssc m)

-- type TmpRealMode ssc = NodeDBHolder ssc (ResourceT IO)

-- runTmpRealMode :: NodeDB ssc -> TmpRealMode ssc a -> IO a
-- runTmpRealMode db = runResourceT . runNodeDBHolder db
