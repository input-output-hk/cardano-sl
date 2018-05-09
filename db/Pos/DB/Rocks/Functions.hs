{-# LANGUAGE RankNTypes #-}

-- | Functions related to rocksdb implementation of database
-- interface.

module Pos.DB.Rocks.Functions
       (
       -- * Opening and modifications
         openRocksDB
       , closeRocksDB
       , openNodeDBs
       , closeNodeDBs
       , usingReadOptions
       , usingWriteOptions

       -- * Reading/writing
       , rocksGetBytes
       , rocksPutBytes
       , rocksDelete
       , rocksPutBi

       -- * Iteration
       , rocksIterSource

       -- * Default methods
       , dbGetDefault
       , dbIterSourceDefault
       , dbPutDefault
       , dbWriteBatchDefault
       , dbDeleteDefault
       ) where

import           Universum

import           Control.Lens (ASetter')
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Conduit (ConduitT, bracketP, yield)
import qualified Database.RocksDB as Rocks
import           System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                                   removeDirectoryRecursive)
import           System.FilePath ((</>))

import           Pos.Binary.Class (Bi)
import           Pos.Core.Configuration (HasCoreConfiguration)
import           Pos.DB.BatchOp (rocksWriteBatch)
import           Pos.DB.Class (DBIteratorClass (..), DBTag (..), IterType)
import           Pos.DB.Functions (dbSerializeValue, processIterEntry)
import           Pos.DB.Rocks.Types (DB (..), MonadRealDB, NodeDBs (..), getDBByTag)
import qualified Pos.Util.Concurrent.RWLock as RWL
import           Pos.Util.Util (lensOf)

----------------------------------------------------------------------------
-- Opening/options
----------------------------------------------------------------------------

openRocksDB :: MonadIO m => FilePath -> m DB
openRocksDB fp = DB Rocks.defaultReadOptions Rocks.defaultWriteOptions options
                   <$> Rocks.open options
  where options = (Rocks.defaultOptions fp)
          { Rocks.optionsCreateIfMissing = True
          , Rocks.optionsCompression     = Rocks.NoCompression
          }

closeRocksDB :: MonadIO m => DB -> m ()
closeRocksDB = Rocks.close . rocksDB

-- | Open all DBs stored on disk.
-- Don't forget to use 'closeNodeDBs' eventually.
openNodeDBs
    :: (MonadIO m)
    => Bool -> FilePath -> m NodeDBs
openNodeDBs recreate fp = do
    liftIO $
        whenM ((recreate &&) <$> doesDirectoryExist fp) $
        removeDirectoryRecursive fp
    let blocksDir = fp </> "blocks"
    let blocksIndexPath = blocksDir </> "index"
    let _blockDataDir = blocksDir </> "data"
    let gStatePath = fp </> "gState"
    let lrcPath = fp </> "lrc"
    let miscPath = fp </> "misc"
    mapM_ ensureDirectoryExists
        [ blocksDir
        , _blockDataDir
        , blocksIndexPath
        , gStatePath
        , lrcPath
        , miscPath
        ]
    _blockIndexDB <- openRocksDB blocksIndexPath
    _gStateDB <- openRocksDB gStatePath
    _lrcDB <- openRocksDB lrcPath
    _miscDB <- openRocksDB miscPath
    _miscLock <- RWL.new
    pure NodeDBs {..}
  where
    ensureDirectoryExists :: MonadIO m => FilePath -> m ()
    ensureDirectoryExists = liftIO . createDirectoryIfMissing True

-- | Safely close all databases from 'NodeDBs'.
closeNodeDBs :: MonadIO m => NodeDBs -> m ()
closeNodeDBs NodeDBs {..} =
    mapM_ closeRocksDB [_blockIndexDB, _gStateDB, _lrcDB, _miscDB]

usingReadOptions
    :: MonadRealDB ctx m
    => Rocks.ReadOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingReadOptions opts l =
    local (over (lensOf @NodeDBs . l) (\db -> db {rocksReadOpts = opts}))

usingWriteOptions
    :: MonadRealDB ctx m
    => Rocks.WriteOptions
    -> ASetter' NodeDBs DB
    -> m a
    -> m a
usingWriteOptions opts l =
    local (over (lensOf @NodeDBs . l) (\db -> db {rocksWriteOpts = opts}))

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
rocksPutBi :: (HasCoreConfiguration, Bi v, MonadIO m) => ByteString -> v -> DB -> m ()
rocksPutBi k v = rocksPutBytes k (dbSerializeValue v)

----------------------------------------------------------------------------
-- Snapshot
----------------------------------------------------------------------------

-- The following is not used in the project yet. So the
-- rocksdb-haskell-ng binding doesn't include it yet, and we simply
-- comment it out here, to be added back at a later stage when needed.

{-
newtype Snapshot = Snapshot Rocks.Snapshot

usingSnapshot
    :: (MonadIO m, MonadMask m)
    => DB -> (Snapshot -> m a) -> m a
usingSnapshot DB {..} action =
    bracket
        (Rocks.createSnapshot rocksDB)
        (Rocks.releaseSnapshot rocksDB)
        (action . Snapshot)
-}

----------------------------------------------------------------------------
-- Iteration
----------------------------------------------------------------------------

-- | Conduit source built from rocks iterator.
rocksIterSource ::
       forall ctx m i.
       ( MonadResource m
       , MonadRealDB ctx m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       , HasCoreConfiguration
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
rocksIterSource tag _ = do
    DB{..} <- lift $ getDBByTag tag
    let createIter = Rocks.createIter rocksDB rocksReadOpts
    let releaseIter i = Rocks.releaseIter i
    let action iter = do
            Rocks.iterSeek iter (iterKeyPrefix @i)
            produce iter
    bracketP createIter releaseIter action
  where
    produce :: Rocks.Iterator -> ConduitT () (IterType i) m ()
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
        -> ConduitT () (IterType i) m (Maybe (IterType i))
    processRes Nothing   = pure Nothing
    processRes (Just kv) = processIterEntry @i kv

----------------------------------------------------------------------------
-- Implementation/default methods
----------------------------------------------------------------------------

dbGetDefault :: MonadRealDB ctx m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetDefault tag key = getDBByTag tag >>= rocksGetBytes key

dbPutDefault :: MonadRealDB ctx m => DBTag -> ByteString -> ByteString -> m ()
dbPutDefault tag key val = getDBByTag tag >>= rocksPutBytes key val

dbWriteBatchDefault :: MonadRealDB ctx m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchDefault tag batch = getDBByTag tag >>= rocksWriteBatch batch

dbDeleteDefault :: MonadRealDB ctx m => DBTag -> ByteString -> m ()
dbDeleteDefault tag key = getDBByTag tag >>= rocksDelete key

dbIterSourceDefault ::
       forall ctx m i.
       ( MonadRealDB ctx m
       , MonadResource m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       , HasCoreConfiguration
       )
    => DBTag
    -> Proxy i
    -> ConduitT () (IterType i) m ()
dbIterSourceDefault = rocksIterSource
