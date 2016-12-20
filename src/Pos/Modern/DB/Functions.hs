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
       , iterateByAllEntries
       ) where

import           Control.Monad.Fail           (fail)
import           Control.Monad.IfElse         (whileM)
import           Control.Monad.TM             ((.>>=.))
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString.Lazy         as BSL
import           Data.Default                 (def)
import qualified Database.RocksDB             as Rocks
import           Formatting                   (formatToString, shown, string, (%))
import           Universum

import           Pos.Binary.Class             (Bi, decodeFull, encodeStrict)
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
       (Bi v, MonadIO m)
    => ByteString -> DB ssc -> m (Maybe v)
rocksGetBi key db = do
    bytes <- rocksGetBytes key db
    bytes .>>=. decodeRocks

decodeRocks :: (Bi v, MonadIO m) => ByteString -> m v
decodeRocks key = either onParseError pure . decodeFull . BSL.fromStrict $ key
  where
    onParseError msg =
        liftIO . fail $
        formatToString
            ("rocksGetBi: stored value is malformed, key = " %shown %
              ", err: " %string)
            key
            msg

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

iterateByAllEntries :: (Bi k, Bi v, MonadMask m, MonadIO m) => DB ssc -> ((k, v) -> m ()) -> m ()
iterateByAllEntries DB{..} callback =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter)
            (\it -> do
                Rocks.iterFirst it
                whileM (Rocks.iterValid it)
                       (do
                            kv <- Rocks.iterEntry it
                            case kv of
                                Nothing     -> pure ()
                                Just (k, v) ->
                                    ((,) <$> decodeRocks k <*> decodeRocks v)
                                    >>= callback
                            Rocks.iterNext it
                       )
             )
