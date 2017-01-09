{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ScopedTypeVariables       #-}

-- | Basically wrappers over RocksDB library.

module Pos.DB.Functions
       ( openDB
       , rocksDelete
       , rocksGetBi
       , rocksGetBytes
       , rocksPutBi
       , rocksPutBytes
       , traverseAllEntries
       , rocksDecodeMaybe
       , rocksDecodeKeyValMaybe

       -- * Batch
       , RocksBatchOp (..)
       , SomeBatchOp (..)
       , rocksWriteBatch
       ) where

import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default                 (def)
import qualified Database.RocksDB             as Rocks
import           Formatting                   (sformat, shown, string, (%))
import           Universum

import           Pos.Binary.Class             (Bi, decodeFull, encodeStrict)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Types                 (DB (..))

-- | Open DB stored on disk.
openDB :: MonadIO m => FilePath -> m (DB ssc)
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
    traverse (rocksDecode . (ToDecodeValue key)) bytes

data ToDecode
    = ToDecodeKey !ByteString
    | ToDecodeValue !ByteString
                    !ByteString

rocksDecode :: (Bi v, MonadThrow m) => ToDecode -> m v
rocksDecode (ToDecodeKey key) =
    either (onParseError key) pure . decodeFull . BSL.fromStrict $ key
rocksDecode (ToDecodeValue key val) =
    either (onParseError key) pure . decodeFull . BSL.fromStrict $ val

onParseError :: (MonadThrow m) => ByteString -> [Char] -> m a
onParseError rawKey errMsg = throwM $ DBMalformed $ sformat fmt rawKey errMsg
  where
    fmt = "rocksGetBi: stored value is malformed, key = "%shown%", err: "%string

rocksDecodeMaybe :: (Bi v) => ByteString -> Maybe v
rocksDecodeMaybe = rightToMaybe . decodeFull . BSL.fromStrict

rocksDecodeKeyVal :: (Bi k, Bi v, MonadThrow m)
                  => (ByteString, ByteString) -> m (k, v)
rocksDecodeKeyVal (k, v) =
    (,) <$> rocksDecode (ToDecodeKey k) <*> rocksDecode (ToDecodeValue k v)

rocksDecodeKeyValMaybe
    :: (Bi k, Bi v)
    => (ByteString, ByteString) -> Maybe (k, v)
rocksDecodeKeyValMaybe (k, v) = (,) <$> rocksDecodeMaybe k <*> rocksDecodeMaybe v

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB ssc -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB ssc -> m ()
rocksPutBi k v = rocksPutBytes k (encodeStrict v)

rocksDelete :: (MonadIO m) => ByteString -> DB ssc -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k

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
                traverse rocksDecodeKeyVal kv `catch` \(_ :: DBError) -> step
            run b = step >>= maybe (pure b) (uncurry (folder b) >=> run)
        init >>= run

----------------------------------------------------------------------------
-- Batch
----------------------------------------------------------------------------

class RocksBatchOp a where
    toBatchOp :: a -> [Rocks.BatchOp]

data SomeBatchOp =
    forall a. RocksBatchOp a =>
              SomeBatchOp a

instance RocksBatchOp Rocks.BatchOp where
    toBatchOp = pure

instance RocksBatchOp SomeBatchOp where
    toBatchOp (SomeBatchOp a) = toBatchOp a

-- | Write Batch encapsulation
rocksWriteBatch :: (RocksBatchOp a, MonadIO m) => [a] -> DB ssc -> m ()
rocksWriteBatch batch DB {..} =
    Rocks.write rocksDB rocksWriteOpts (concatMap toBatchOp batch)
