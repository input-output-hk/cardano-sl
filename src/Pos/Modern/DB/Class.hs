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
       , getMiscDB
       ) where

import           Control.Lens         (ASetter', view)
import           Control.TimeWarp.Rpc (ResponseT (..))
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.DHT.Model        (DHTResponseT (..))
import           Pos.DHT.Real         (KademliaDHT (..))
import           Pos.Modern.DB.Types  (DB, NodeDBs, blockDB, miscDB, utxoDB)

-- TODO write a documentation. LensLike' is just a lens. Written using
-- LensLike' to avoid rankntypes.
class MonadIO m => MonadDB ssc m | m -> ssc where
    getNodeDBs :: m (NodeDBs ssc)
    usingReadOptions :: Rocks.ReadOptions -> ASetter' (NodeDBs ssc) (DB ssc) -> m a -> m a
    usingWriteOptions :: Rocks.WriteOptions -> ASetter' (NodeDBs ssc) (DB ssc) -> m a -> m a

getBlockDB :: MonadDB ssc m => m (DB ssc)
getBlockDB = view blockDB <$> getNodeDBs

getUtxoDB :: MonadDB ssc m => m (DB ssc)
getUtxoDB = view utxoDB <$> getNodeDBs

getMiscDB :: MonadDB ssc m => m (DB ssc)
getMiscDB = view miscDB <$> getNodeDBs

instance (MonadDB ssc m) => MonadDB ssc (ReaderT a m) where
    getNodeDBs = lift getNodeDBs
    usingReadOptions how l m =
        ask >>= lift . usingReadOptions how l . runReaderT m
    usingWriteOptions how l m =
        ask >>= lift . usingWriteOptions how l . runReaderT m

deriving instance (MonadDB ssc m) => MonadDB ssc (ResponseT s m)
deriving instance (MonadDB ssc m) => MonadDB ssc (KademliaDHT m)
deriving instance (MonadDB ssc m) => MonadDB ssc (DHTResponseT s m)
