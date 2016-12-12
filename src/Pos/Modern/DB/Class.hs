{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE StandaloneDeriving     #-}
{-# LANGUAGE UndecidableInstances   #-}

-- | Class which provides access to database.

module Pos.Modern.DB.Class
       ( MonadDB (..)
       , getBlockDB
       , getUtxoDB
       ) where

import           Control.Lens         (view)
import           Control.TimeWarp.Rpc (ResponseT (..))
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.DHT              (DHTResponseT (..))
import           Pos.DHT.Real         (KademliaDHT (..))
import           Pos.Modern.DB.Types  (DB, NodeDBs, blockDB, utxoDB)

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

instance (MonadDB ssc m) =>
         MonadDB ssc (ReaderT a m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptionsUtxo how m =
        ask >>= lift . usingReadOptionsUtxo how . runReaderT m
    usingWriteOptionsUtxo how m =
        ask >>= lift . usingWriteOptionsUtxo how . runReaderT m
    usingReadOptionsBlock how m =
        ask >>= lift . usingReadOptionsBlock how . runReaderT m
    usingWriteOptionsBlock how m =
        ask >>= lift . usingWriteOptionsBlock how . runReaderT m

deriving instance (MonadDB ssc m) => MonadDB ssc (ResponseT m)
deriving instance (MonadDB ssc m) => MonadDB ssc (KademliaDHT m)
deriving instance (MonadDB ssc m) => MonadDB ssc (DHTResponseT m)
