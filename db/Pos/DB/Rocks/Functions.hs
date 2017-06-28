{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Functions related to rocksdb implementation of database
-- interface.

module Pos.DB.Rocks.Functions
       (
       -- * Opening and modifications
         openRocksDB
       , closeRocksDB
       , usingReadOptions
       , usingWriteOptions

       -- * Reading/writing
       , rocksGetBytes
       , rocksPutBytes
       , rocksDelete
       , rocksPutBi

       -- * Snapshot
       , Snapshot (..)
       , usingSnapshot

       -- * Iteration
       , rocksIterSource
       ) where

import           Universum

import           Control.Lens                 (ASetter')
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit                 (ConduitM, Source, bracketP, yield)
import           Data.Default                 (def)
import qualified Database.RocksDB             as Rocks
import qualified Ether

import           Pos.Binary.Class             (Bi, encode)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag (..), IterType)
import           Pos.DB.Functions             (processIterEntry)
import           Pos.DB.Rocks.Types           (DB (..), MonadRealDB, NodeDBs, getDBByTag)

----------------------------------------------------------------------------
-- Opening/options
----------------------------------------------------------------------------

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB fp = DB def def def
                   <$> Rocks.open fp def
                        { Rocks.createIfMissing = True
                        , Rocks.compression     = Rocks.NoCompression
                        }


closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

usingReadOptions
    :: MonadRealDB m
    => Rocks.ReadOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingReadOptions opts l =
    Ether.local' (over l (\db -> db {rocksReadOpts = opts}))

usingWriteOptions
    :: MonadRealDB m
    => Rocks.WriteOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingWriteOptions opts l =
    Ether.local' (over l (\db -> db {rocksWriteOpts = opts}))

----------------------------------------------------------------------------
-- Reading/writing
----------------------------------------------------------------------------

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Delete element from RocksDB for given key.
rocksDelete :: (MonadIO m) => ByteString -> DB -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k

-- garbage, should be abstracted and hidden

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB -> m ()
rocksPutBi k v = rocksPutBytes k (encode v)

----------------------------------------------------------------------------
-- Snapshot
----------------------------------------------------------------------------

newtype Snapshot = Snapshot Rocks.Snapshot

usingSnapshot
    :: (MonadIO m, MonadMask m)
    => DB -> (Snapshot -> m a) -> m a
usingSnapshot DB {..} action =
    bracket
        (Rocks.createSnapshot rocksDB)
        (Rocks.releaseSnapshot rocksDB)
        (action . Snapshot)

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Conduit source built from rocks iterator.
rocksIterSource ::
       forall m i.
       ( MonadResource m
       , MonadRealDB m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> Source m (IterType i)
rocksIterSource tag _ = do
    putText $ ("Iterator source, prefix " <> show (iterKeyPrefix @i))
    DB{..} <- lift $ getDBByTag tag
    let createIter = Rocks.createIter rocksDB rocksReadOpts
    let releaseIter i = Rocks.releaseIter i
    let onExc (e :: SomeException) = do
            putText $ "Exception arised in redirect handler: " <> show e
            throwM e
    let action iter = do
            Rocks.iterSeek iter (iterKeyPrefix @i)
            produce iter `catch` onExc
    bracketP createIter releaseIter $ \i -> action i `catch` onExc
  where
    produce :: Rocks.Iterator -> Source m (IterType i)
    produce it = do
        entryStr <- processRes =<< Rocks.iterEntry it
        case entryStr of
            Nothing -> pass
            Just e -> do
                yield e
                Rocks.iterNext it
                produce it
    processRes ::
           (Bi (IterKey i), Bi (IterValue i))
        => Maybe (ByteString, ByteString)
        -> ConduitM () (IterType i) m (Maybe (IterType i))
    processRes Nothing   = pure Nothing
    processRes (Just kv) = processIterEntry @i kv
