{-# LANGUAGE TypeFamilies #-}

-- | Class which provides access to database.

module Pos.DB.Class
       ( MonadDB
       , getNodeDBs
       , usingReadOptions
       , usingWriteOptions
       , getBlockIndexDB
       , getGStateDB
       , getLrcDB
       , getMiscDB

       -- * GState Core
       , MonadGStateCore (..)
       , gsMaxBlockSize
       , gsMaxHeaderSize
       , gsMaxTxSize
       , gsMaxProposalSize
       , MonadDBCore
       ) where

import           Universum

import           Control.Lens                   (ASetter')
import           Control.Monad.Trans            (MonadTrans (..))
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))
import qualified Database.RocksDB               as Rocks
import qualified Ether
import           Serokell.Data.Memory.Units     (Byte)

import           Pos.Core                       (BlockVersionData (..))
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

getGStateDB :: MonadDB m => m DB
getGStateDB = view gStateDB <$> getNodeDBs

getLrcDB :: MonadDB m => m DB
getLrcDB = view lrcDB <$> getNodeDBs

getMiscDB :: MonadDB m => m DB
getMiscDB = view miscDB <$> getNodeDBs

-- | This type class provides functions to get core data from GState.
-- The idea is that actual getters may be defined at high levels, but
-- may be needed at lower levels.
--
-- This class doesn't have a 'MonadDB' constraint, because alternative
-- DBs my be used to provide this data. There is also 'MonadDBCore' constraint
-- which unites 'MonadDB' and 'GStateCore'.
class Monad m => MonadGStateCore m where
    gsAdoptedBVData :: m BlockVersionData

instance {-# OVERLAPPABLE #-}
    (MonadGStateCore m, MonadTrans t, LiftLocal t,
     Monad (t m)) =>
        MonadGStateCore (t m)
  where
    gsAdoptedBVData = lift gsAdoptedBVData

gsMaxBlockSize :: MonadGStateCore m => m Byte
gsMaxBlockSize = bvdMaxBlockSize <$> gsAdoptedBVData

gsMaxHeaderSize :: MonadGStateCore m => m Byte
gsMaxHeaderSize = bvdMaxHeaderSize <$> gsAdoptedBVData

gsMaxTxSize :: MonadGStateCore m => m Byte
gsMaxTxSize = bvdMaxTxSize <$> gsAdoptedBVData

gsMaxProposalSize :: MonadGStateCore m => m Byte
gsMaxProposalSize = bvdMaxProposalSize <$> gsAdoptedBVData

type MonadDBCore m = (MonadDB m, MonadGStateCore m)
