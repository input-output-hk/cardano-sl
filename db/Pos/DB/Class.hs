{-# LANGUAGE TypeFamilies #-}

-- | Class which provides access to database.

module Pos.DB.Class
       ( MonadDB
       , getNodeDBs
       , usingReadOptions
       , usingWriteOptions
       , getBlockIndexDB
       , getUtxoDB
       , getLrcDB
       , getMiscDB
       , MonadDBCore(..)
       ) where

import           Universum

import           Control.Lens                   (ASetter')
import           Control.Monad.Trans            (MonadTrans (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import qualified Database.RocksDB               as Rocks
import qualified Ether

import           Pos.Core                       (BlockVersionData)
import           Pos.DB.Types                   (DB (..), NodeDBs, blockIndexDB, gStateDB,
                                                 lrcDB, miscDB)

type MonadDB m = (Ether.MonadReader' NodeDBs m, MonadIO m, MonadCatch m)

getNodeDBs :: MonadDB m => m NodeDBs
getNodeDBs = Ether.ask'

usingReadOptions
    :: MonadDB m
    => Rocks.ReadOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingReadOptions opts l =
    Ether.local' (over l (\db -> db {rocksReadOpts = opts}))

usingWriteOptions
    :: MonadDB m
    => Rocks.WriteOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingWriteOptions opts l =
    Ether.local' (over l (\db -> db {rocksWriteOpts = opts}))

getBlockIndexDB :: MonadDB m => m DB
getBlockIndexDB = view blockIndexDB <$> getNodeDBs

getUtxoDB :: MonadDB m => m DB
getUtxoDB = view gStateDB <$> getNodeDBs

getLrcDB :: MonadDB m => m DB
getLrcDB = view lrcDB <$> getNodeDBs

getMiscDB :: MonadDB m => m DB
getMiscDB = view miscDB <$> getNodeDBs

-- | This type class provides functions to get core data from DB.
class MonadDB m => MonadDBCore m where
    dbAdoptedBVData :: m BlockVersionData

instance {-# OVERLAPPABLE #-}
    (MonadDBCore m, MonadTrans t, LiftLocal t,
     MonadIO (t m), MonadCatch (t m)) =>
        MonadDBCore (t m)
  where
    dbAdoptedBVData = lift dbAdoptedBVData
