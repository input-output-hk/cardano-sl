{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Different key/value serialization helpers abstracted over
-- 'MonadDB'.

module Pos.DB.Functions
       (
       -- * Encoded putting/getting
         dbGetBi
       , dbPutBi

       -- * Decoding/encoding primitives and iteration related
       , dbDecode
       , dbDecodeWP
       , dbDecodeMaybe
       , dbDecodeMaybeWP
       , encodeWithKeyPrefix
       , processIterEntry
       ) where

import           Universum

import qualified Data.ByteString  as BS (drop, isPrefixOf)
import           Formatting       (sformat, shown, stext, string, (%))

import           Pos.Binary.Class (Bi, decodeFull, encode)
import           Pos.DB.Class     (DBIteratorClass (..), DBTag, IterType, MonadDB (..),
                                   MonadDBRead (..))
import           Pos.DB.Error     (DBError (DBMalformed))
import           Pos.Util.Util    (maybeThrow)


-- | Read serialized value associated with given key from pure DB.
dbGetBi
    :: forall v m.
       (Bi v, MonadDBRead m)
    => DBTag -> ByteString -> m (Maybe v)
dbGetBi tag key = do
    bytes <- dbGet tag key
    traverse (dbDecode . (ToDecodeValue key)) bytes

-- | Write serializable value to DB for given key.
dbPutBi :: (Bi v, MonadDB m) => DBTag -> ByteString -> v -> m ()
dbPutBi tag k v = dbPut tag k (encode v)


data ToDecode
    = ToDecodeKey !ByteString
    | ToDecodeValue !ByteString
                    !ByteString

dbDecode :: (Bi v, MonadThrow m) => ToDecode -> m v
dbDecode (ToDecodeKey key) =
    either (onParseError key) pure . decodeFull $ key
dbDecode (ToDecodeValue key val) =
    either (onParseError key) pure . decodeFull $ val

onParseError :: (MonadThrow m) => ByteString -> Text -> m a
onParseError rawKey errMsg = throwM $ DBMalformed $ sformat fmt rawKey errMsg
  where
    fmt = "rocksGetBi: stored value is malformed, key = "%shown%", err: "%stext

-- with prefix
dbDecodeWP
    :: forall i m . (MonadThrow m, DBIteratorClass i, Bi (IterKey i))
    => ByteString -> m (IterKey i)
dbDecodeWP key
    | BS.isPrefixOf (iterKeyPrefix @i) key =
        either (onParseError key) pure .
        decodeFull .
        BS.drop (length $ iterKeyPrefix @i) $
        key
    | otherwise = onParseError key "unexpected prefix"

dbDecodeMaybe :: (Bi v) => ByteString -> Maybe v
dbDecodeMaybe = rightToMaybe . decodeFull

-- Parse maybe
dbDecodeMaybeWP
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => ByteString -> Maybe (IterKey i)
dbDecodeMaybeWP s
    | BS.isPrefixOf (iterKeyPrefix @i) s =
          rightToMaybe .
          decodeFull .
          BS.drop (length $ iterKeyPrefix @i) $ s
    | otherwise = Nothing

-- | Encode iterator key using iterator prefix defined in
-- 'DBIteratorClass'.
encodeWithKeyPrefix
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => IterKey i -> ByteString
encodeWithKeyPrefix = (iterKeyPrefix @i <>) . encode

-- | Given a @(k,v)@ as pair of strings, try to decode both.
processIterEntry ::
       forall i m.
       (Bi (IterKey i), Bi (IterValue i), MonadThrow m, DBIteratorClass i)
    => (ByteString, ByteString)
    -> m (Maybe (IterType i))
processIterEntry (key,val)
    | BS.isPrefixOf prefix key = do
        k <- maybeThrow (DBMalformed $ fmt key "key invalid")
                        (dbDecodeMaybeWP @i key)
        v <- maybeThrow (DBMalformed $ fmt key "value invalid")
                        (dbDecodeMaybe val)
        pure $ Just (k, v)
    | otherwise = pure Nothing
  where
    prefix = iterKeyPrefix @i
    fmt k err =
      sformat
          ("Iterator entry with keyPrefix = "%shown%" is malformed: \
           \key = "%shown%", err: " %string)
           prefix k err
