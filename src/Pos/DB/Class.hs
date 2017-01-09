{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to database.

module Pos.DB.Class
       ( MonadDB (..)
       , getBlockDB
       , getUtxoDB
       , getLrcDB
       , getMiscDB
       ) where

import           Control.Lens         (ASetter', view)
import           Control.Monad.Except (ExceptT (..), mapExceptT)
import           Control.Monad.State  (StateT (..), get)
import           Control.TimeWarp.Rpc (ResponseT (..))
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.DB.Types         (DB, NodeDBs, blockDB, gStateDB, lrcDB, miscDB)
import           Pos.DHT.Model        (DHTResponseT (..))
import           Pos.DHT.Real         (KademliaDHT (..))
import           Pos.Util.Iterator    (ListHolderT (..))

-- TODO write a documentation. LensLike' is just a lens. Written using
-- LensLike' to avoid rankntypes.
class (MonadIO m, MonadThrow m) => MonadDB ssc m | m -> ssc where
    getNodeDBs :: m (NodeDBs ssc)
    usingReadOptions :: Rocks.ReadOptions -> ASetter' (NodeDBs ssc) (DB ssc) -> m a -> m a
    usingWriteOptions :: Rocks.WriteOptions -> ASetter' (NodeDBs ssc) (DB ssc) -> m a -> m a

getBlockDB :: MonadDB ssc m => m (DB ssc)
getBlockDB = view blockDB <$> getNodeDBs

getUtxoDB :: MonadDB ssc m => m (DB ssc)
getUtxoDB = view gStateDB <$> getNodeDBs

getLrcDB :: MonadDB ssc m => m (DB ssc)
getLrcDB = view lrcDB <$> getNodeDBs

getMiscDB :: MonadDB ssc m => m (DB ssc)
getMiscDB = view miscDB <$> getNodeDBs

instance (MonadDB ssc m) => MonadDB ssc (ReaderT a m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l m =
        ask >>= lift . usingReadOptions how l . runReaderT m
    usingWriteOptions how l m =
        ask >>= lift . usingWriteOptions how l . runReaderT m

instance (MonadDB ssc m) => MonadDB ssc (ExceptT e m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l = mapExceptT (usingReadOptions how l)
    usingWriteOptions how l = mapExceptT (usingWriteOptions how l)

instance (MonadDB ssc m) => MonadDB ssc (StateT a m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l m =
        get >>= lift . usingReadOptions how l . evalStateT m
    usingWriteOptions how l m =
        get >>= lift . usingWriteOptions how l . evalStateT m

deriving instance (MonadDB ssc m) => MonadDB ssc (ResponseT s m)
deriving instance (MonadDB ssc m) => MonadDB ssc (KademliaDHT m)
deriving instance (MonadDB ssc m) => MonadDB ssc (DHTResponseT s m)
deriving instance (MonadDB ssc m) => MonadDB ssc (ListHolderT s m)
