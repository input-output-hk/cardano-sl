{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

-- | Class which provides access to database.

module Pos.Modern.DB.Class
       ( MonadDB (..)
       , getBlockDB
       , getUtxoDB
       ) where

import           Control.Lens        (view)
import qualified Database.RocksDB    as Rocks
import           Universum

import           Pos.Modern.DB.Types (DB, NodeDBs, blockDB, utxoDB)

class MonadIO m => MonadDB ssc m | m -> ssc where
    getNodeDBs :: m (NodeDBs ssc)
    usingReadOptionsUtxo   :: Rocks.ReadOptions -> m a -> m a
    usingWriteOptionsUtxo  :: Rocks.WriteOptions -> m a -> m a
    usingReadOptionsBlock  :: Rocks.ReadOptions -> m a -> m a
    usingWriteOptionsBlock :: Rocks.WriteOptions -> m a -> m a

getBlockDB :: MonadDB ssc m => m (DB ssc)
getBlockDB = view blockDB <$> getNodeDBs

getUtxoDB :: MonadDB ssc m => m (DB ssc)
getUtxoDB = view utxoDB <$> getNodeDBs
