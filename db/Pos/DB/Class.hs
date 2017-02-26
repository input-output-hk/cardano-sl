{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to database.

module Pos.DB.Class
       ( MonadDB (..)
       , getBlockDB
       , getUtxoDB
       , getLrcDB
       , getMiscDB
       ) where

import           Control.Lens                 (ASetter')
import           Control.Monad.Except         (ExceptT (..), mapExceptT)
import           Control.Monad.Reader         (mapReaderT)
import           Control.Monad.State          (StateT (..), mapStateT)
import           Control.Monad.Trans.Resource (ResourceT, transResourceT)
import qualified Database.RocksDB             as Rocks
import           Universum

import           Pos.DB.Types                 (DB, NodeDBs, blockDB, gStateDB, lrcDB,
                                               miscDB)
import           Pos.Util.Iterator            (ListHolderT (..))

-- TODO write a documentation. LensLike' is just a lens. Written using
-- LensLike' to avoid rankntypes.
class (MonadIO m, MonadThrow m) => MonadDB m where
    getNodeDBs :: m NodeDBs
    usingReadOptions :: Rocks.ReadOptions -> ASetter' NodeDBs DB -> m a -> m a
    usingWriteOptions :: Rocks.WriteOptions -> ASetter' NodeDBs DB -> m a -> m a

getBlockDB :: MonadDB m => m DB
getBlockDB = view blockDB <$> getNodeDBs

getUtxoDB :: MonadDB m => m DB
getUtxoDB = view gStateDB <$> getNodeDBs

getLrcDB :: MonadDB m => m DB
getLrcDB = view lrcDB <$> getNodeDBs

getMiscDB :: MonadDB m => m DB
getMiscDB = view miscDB <$> getNodeDBs

instance (MonadDB m) => MonadDB (ReaderT a m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l = mapReaderT (usingReadOptions how l)
    usingWriteOptions how l = mapReaderT (usingWriteOptions how l)

instance (MonadDB m) => MonadDB (ExceptT e m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l = mapExceptT (usingReadOptions how l)
    usingWriteOptions how l = mapExceptT (usingWriteOptions how l)

instance (MonadDB m) => MonadDB (StateT a m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l = mapStateT (usingReadOptions how l)
    usingWriteOptions how l = mapStateT (usingWriteOptions how l)

instance (MonadDB m) => MonadDB (ResourceT m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l = transResourceT (usingReadOptions how l)
    usingWriteOptions how l = transResourceT (usingWriteOptions how l)

deriving instance (MonadDB m) => MonadDB (ListHolderT s m)
