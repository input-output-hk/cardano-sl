{-# LANGUAGE ScopedTypeVariables #-}

-- | Basically wrappers over RocksDB library.

module Pos.Modern.DB.Functions
       ( openDB
       , rocksDelete
       , rocksGetBi
       , rocksGetBytes
       , rocksPutBi
       , rocksPutBytes
       , rocksWriteBatch
       , traverseAllEntries
       , rocksDecode
       ) where

import           Control.Monad.TM             ((.>>=.))
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default                 (def)
import qualified Database.RocksDB             as Rocks
import           Formatting                   (sformat, shown, string, (%))
import           Universum

import           Pos.Binary.Class             (Bi, decodeFull, encodeStrict)
import           Pos.Modern.DB.Error          (DBError (DBMalformed))
import           Pos.Modern.DB.Types          (DB (..))

-- | Open DB stored on disk.
openDB :: MonadResource m => FilePath -> m (DB ssc)
openDB fp = DB def def def
                   <$> Rocks.open fp def { Rocks.createIfMissing = True }

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB ssc -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- | Read serialized value from RocksDB using given key.
rocksGetBi
    :: forall v m ssc.
       (Bi v, MonadIO m, MonadThrow m)
    => ByteString -> DB ssc -> m (Maybe v)
rocksGetBi key db = do
    bytes <- rocksGetBytes key db
    bytes .>>=. rocksDecode

rocksDecode :: (Bi v, MonadIO m, MonadThrow m) => ByteString -> m v
rocksDecode key = either onParseError pure . decodeFull . BSL.fromStrict $ key
  where
    onParseError msg =
        throwM $ DBMalformed $
        sformat
            ("rocksGetBi: stored value is malformed, key = " %shown %
              ", err: " %string)
            key
            msg

rocksDecodeKeyVal :: (Bi k, Bi v, MonadIO m, MonadThrow m)
                  => (ByteString, ByteString) -> m (k, v)
rocksDecodeKeyVal (k, v) = (,) <$> rocksDecode k <*> rocksDecode v

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB ssc -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB ssc -> m ()
rocksPutBi k v = rocksPutBytes k (encodeStrict v)

rocksDelete :: (MonadIO m) => ByteString -> DB ssc -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k

-- | Write Batch incapsulation
rocksWriteBatch :: MonadIO m => [Rocks.BatchOp] -> DB ssc -> m ()
rocksWriteBatch batch DB{..} = Rocks.write rocksDB rocksWriteOpts batch

traverseAllEntries
    :: (Bi k, Bi v, MonadMask m, MonadIO m)
    => DB ssc
    -> m b
    -> (b -> k -> v -> m b)
    -> m b
traverseAllEntries DB{..} init folder =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter) $
    \it -> do
        Rocks.iterFirst it
        let step = do
                kv <- Rocks.iterEntry it
                Rocks.iterNext it
                traverse rocksDecodeKeyVal kv
            run b = step >>= maybe (pure b) (uncurry (folder b) >=> run)
        init >>= run
