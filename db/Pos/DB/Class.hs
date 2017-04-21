{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to database.

module Pos.DB.Class
       ( MonadDB (..)
       , getBlockIndexDB
       , getUtxoDB
       , getLrcDB
       , getMiscDB
       , MonadDBCore (..)
       ) where

import           Universum

import           Control.Lens                 (ASetter')
import           Control.Monad.Trans          (MonadTrans)
import qualified Database.RocksDB             as Rocks

import           Pos.Core                     (BlockVersionData)
import           Pos.DB.Types                 (DB, NodeDBs, blockIndexDB, gStateDB, lrcDB,
                                               miscDB)
import           Pos.Util.Iterator            (ListHolderT (..))
import           Mockable.Class               (MFunctor'(..))

-- TODO write a documentation. LensLike' is just a lens. Written using
-- LensLike' to avoid rankntypes.
class (MonadIO m, MonadCatch m) => MonadDB m where
    getNodeDBs :: m NodeDBs
    usingReadOptions :: Rocks.ReadOptions -> ASetter' NodeDBs DB -> m a -> m a
    usingWriteOptions :: Rocks.WriteOptions -> ASetter' NodeDBs DB -> m a -> m a

    default getNodeDBs :: (MonadTrans t, MonadDB m', t m' ~ m) => m NodeDBs
    getNodeDBs = lift getNodeDBs

    default usingReadOptions
      :: (MFunctor' t m' m', MonadDB m', t m' ~ m)
      => Rocks.ReadOptions
      -> ASetter' NodeDBs DB
      -> m a
      -> m a
    usingReadOptions how l = hoist' (usingReadOptions how l)

    default usingWriteOptions
      :: (MFunctor' t m' m', MonadDB m', t m' ~ m)
      => Rocks.WriteOptions
      -> ASetter' NodeDBs DB
      -> m a
      -> m a
    usingWriteOptions how l = hoist' (usingWriteOptions how l)

getBlockIndexDB :: MonadDB m => m DB
getBlockIndexDB = view blockIndexDB <$> getNodeDBs

getUtxoDB :: MonadDB m => m DB
getUtxoDB = view gStateDB <$> getNodeDBs

getLrcDB :: MonadDB m => m DB
getLrcDB = view lrcDB <$> getNodeDBs

getMiscDB :: MonadDB m => m DB
getMiscDB = view miscDB <$> getNodeDBs

instance {-# OVERLAPPABLE #-}
    (MonadDB m, MFunctor' t m m, MonadTrans t, MonadIO (t m), MonadCatch (t m)) =>
    MonadDB (t m)

deriving instance (MonadDB m) => MonadDB (ListHolderT s m)

-- | This type class provides functions to get core data from DB.
class MonadDB m => MonadDBCore m where
    dbAdoptedBVData :: m BlockVersionData

    default dbAdoptedBVData :: (MonadTrans t, MonadDBCore m', t m' ~ m) =>
        m BlockVersionData
    dbAdoptedBVData = lift dbAdoptedBVData

instance {-# OVERLAPPABLE #-}
  (MonadDBCore m, MFunctor' t m m, MonadTrans t, MonadIO (t m), MonadCatch (t m)) =>
  MonadDBCore (t m)
