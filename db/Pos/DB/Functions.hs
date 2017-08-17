{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Different key/value serialization helpers abstracted over
-- 'MonadDB'.

module Pos.DB.Functions
       (
       -- * Encoded putting/getting
         dbGetBi
       , dbPutBi

       -- * Decoding/encoding primitives and iteration related
       , dbDecode
       , dbDecodeMaybe
       , encodeWithKeyPrefix
       , processIterEntry
       ) where

import           Universum

import qualified Data.ByteString  as BS (drop, isPrefixOf)
import           Formatting       (bprint, builder, sformat, shown, stext, string, (%))

import           Pos.Binary.Class (Bi, decodeFull, serialize')
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
dbPutBi tag k v = dbPut tag k (serialize' v)

-- This type describes what we want to decode and contains auxiliary
-- data.
data ToDecode
    = ToDecodeKey !ByteString   -- key
    | ToDecodeValue !ByteString -- key
                    !ByteString -- value

dbDecode :: forall v m. (Bi v, MonadThrow m) => ToDecode -> m v
dbDecode =
    \case
        ToDecodeKey key ->
            either (onParseError key Nothing) pure . decodeFull $ key
        ToDecodeValue key val ->
            either (onParseError key (Just val)) pure . decodeFull $ val
  where
    onParseError :: ByteString -> Maybe ByteString -> Text -> m a
    onParseError rawKey rawValMaybe errMsg =
        let valueBuilder = maybe "" (bprint (", value = " %shown)) rawValMaybe
        in throwM $ DBMalformed $ sformat fmt rawKey valueBuilder errMsg
    fmt =
        "A key or value stored in DB is malformed, key = "%shown%
        builder%
        ", err: "%stext

dbDecodeMaybe :: (Bi v) => ByteString -> Maybe v
dbDecodeMaybe = rightToMaybe . decodeFull

-- Parse maybe
dbDecodeMaybeWP
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => ByteString -> Maybe (IterKey i)
dbDecodeMaybeWP s
    | BS.isPrefixOf (iterKeyPrefix @i) s =
        dbDecodeMaybe . BS.drop (length $ iterKeyPrefix @i) $ s
    | otherwise = Nothing

-- | Encode iterator key using iterator prefix defined in
-- 'DBIteratorClass'.
encodeWithKeyPrefix
    :: forall i . (DBIteratorClass i, Bi (IterKey i))
    => IterKey i -> ByteString
encodeWithKeyPrefix = (iterKeyPrefix @i <>) . serialize'

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
