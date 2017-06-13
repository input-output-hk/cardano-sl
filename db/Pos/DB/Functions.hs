{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Basically wrappers over RocksDB library.

module Pos.DB.Functions
       ( openDB
       , closeDB

       -- * Key/Value helpers
       -- ** General
       , encodeWithKeyPrefix
       , dbGetBi
       , dbPutBi

       -- ** RocksDB
       , rocksDelete
       , rocksGetBi
       , rocksGetBytes
       , rocksPutBi
       , rocksPutBytes
       , rocksDecodeWP
       , rocksDecodeMaybe
       , rocksDecodeMaybeWP
       ) where

import           Universum

import qualified Data.ByteString       as BS (drop, isPrefixOf)
import qualified Data.ByteString.Lazy  as BSL
import           Data.Default          (def)
import qualified Database.RocksDB      as Rocks
import           Formatting            (sformat, shown, string, (%))

import           Pos.Binary.Class      (Bi, decodeFull, encodeStrict)
import           Pos.DB.Class          (DBTag, MonadDB (..), MonadDBRead (..))
import           Pos.DB.Error          (DBError (DBMalformed))
import           Pos.DB.Iterator.Class (DBIteratorClass (..))
import           Pos.DB.Types          (DB (..))

openDB :: MonadIO m => FilePath -> m DB
openDB fp = DB def def def
                   <$> Rocks.open fp def
                        { Rocks.createIfMissing = True
                        , Rocks.compression     = Rocks.NoCompression
                        }

closeDB :: MonadIO m => DB -> m ()
closeDB = Rocks.close . rocksDB

encodeWithKeyPrefix
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => IterKey i -> ByteString
encodeWithKeyPrefix = (iterKeyPrefix @i Proxy <>) . encodeStrict

-- | Read ByteString from RocksDb using given key.
rocksGetBytes :: (MonadIO m) => ByteString -> DB -> m (Maybe ByteString)
rocksGetBytes key DB {..} = Rocks.get rocksDB rocksReadOpts key

-- TODO: get rid of duplicated code

-- | Read serialized value associated with given key from pure DB.
dbGetBi
    :: forall v m.
       (Bi v, MonadDBRead m)
    => DBTag -> ByteString -> m (Maybe v)
dbGetBi tag key = do
    bytes <- dbGet tag key
    traverse (rocksDecode . (ToDecodeValue key)) bytes

-- | Write serializable value to DB for given key.
dbPutBi :: (Bi v, MonadDB m) => DBTag -> ByteString -> v -> m ()
dbPutBi tag k v = dbPut tag k (encodeStrict v)

-- | Read serialized value from RocksDB using given key.
rocksGetBi
    :: forall v m.
       (Bi v, MonadIO m, MonadThrow m)
    => ByteString -> DB -> m (Maybe v)
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

onParseError :: (MonadThrow m) => ByteString -> String -> m a
onParseError rawKey errMsg = throwM $ DBMalformed $ sformat fmt rawKey errMsg
  where
    fmt = "rocksGetBi: stored value is malformed, key = "%shown%", err: "%string

-- with prefix
rocksDecodeWP
    :: forall i m . (MonadThrow m, DBIteratorClass i, Bi (IterKey i))
    => ByteString -> m (IterKey i)
rocksDecodeWP key
    | BS.isPrefixOf (iterKeyPrefix @i Proxy) key =
        either (onParseError key) pure .
        decodeFull .
        BSL.fromStrict .
        BS.drop (length $ iterKeyPrefix @i Proxy) $
        key
    | otherwise = onParseError key "unexpected prefix"

-- Parse maybe
rocksDecodeMaybeWP
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => ByteString -> Maybe (IterKey i)
rocksDecodeMaybeWP s
    | BS.isPrefixOf (iterKeyPrefix @i Proxy) s =
          rightToMaybe .
          decodeFull .
          BSL.fromStrict .
          BS.drop (length $ iterKeyPrefix @i Proxy) $ s
    | otherwise = Nothing

rocksDecodeMaybe :: (Bi v) => ByteString -> Maybe v
rocksDecodeMaybe = rightToMaybe . decodeFull . BSL.fromStrict

-- | Write ByteString to RocksDB for given key.
rocksPutBytes :: (MonadIO m) => ByteString -> ByteString -> DB -> m ()
rocksPutBytes k v DB {..} = Rocks.put rocksDB rocksWriteOpts k v

-- | Write serializable value to RocksDb for given key.
rocksPutBi :: (Bi v, MonadIO m) => ByteString -> v -> DB -> m ()
rocksPutBi k v = rocksPutBytes k (encodeStrict v)

rocksDelete :: (MonadIO m) => ByteString -> DB -> m ()
rocksDelete k DB {..} = Rocks.delete rocksDB rocksWriteOpts k
