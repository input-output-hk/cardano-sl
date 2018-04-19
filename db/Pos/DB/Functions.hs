{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Different key/value serialization helpers abstracted over
-- 'MonadDB'.

module Pos.DB.Functions
       (
       -- * Encoded putting/getting
         dbGetBi
       , dbGetBiNoVersion
       , dbPutBi
       , dbPutBiNoVersion
       , dbSerializeValue

       -- * Decoding/encoding primitives and iteration related
       , dbDecode
       , encodeWithKeyPrefix
       , processIterEntry
       ) where

import           Universum

import qualified Data.ByteString as BS (drop, isPrefixOf)
import           Formatting (bprint, builder, sformat, shown, stext, string, (%))

import           Pos.Binary.Class (Bi, decodeFull', serialize')
import           Pos.Core.Configuration (dbSerializeVersion, HasCoreConfiguration)
import           Pos.DB.Class (DBIteratorClass (..), DBTag, IterType, MonadDB (..),
                               MonadDBRead (..))
import           Pos.DB.Error (DBError (..))
import           Pos.Util.Util (maybeThrow)

-- | Read serialized value associated with given key from pure DB.
dbGetBiNoVersion
    :: forall v m.
       (Bi v, MonadDBRead m)
    => DBTag -> ByteString -> m (Maybe v)
dbGetBiNoVersion tag key = do
    bytes <- dbGet tag key
    traverse (dbDecode . (ToDecodeValue key)) bytes

-- | Write serializable value to DB for given key.
dbPutBiNoVersion :: (Bi v, MonadDB m) => DBTag -> ByteString -> v -> m ()
dbPutBiNoVersion tag k v = dbPut tag k (serialize' v)

-- | Read serialized value (with version) associated with given key from pure DB.
dbGetBi
    :: forall v m.
       (Bi v, MonadDBRead m)
    => DBTag -> ByteString -> m (Maybe v)
dbGetBi tag key = do
    bytes <- dbGet tag key
    val <- traverse (dbDecode . (ToDecodeValue key)) bytes
    traverse onVersionError val
  where
    onVersionError :: (Word8, v) -> m v
    onVersionError (verTag, v)
        | verTag /= dbSerializeVersion =
              throwM $ DBUnexpectedVersionTag dbSerializeVersion verTag
        | otherwise = pure v

-- | Write serializable value to DB for given key. Uses simple versioning.
dbPutBi :: (Bi v, MonadDB m) => DBTag -> ByteString -> v -> m ()
dbPutBi tag k v = dbPut tag k (dbSerializeValue v)

-- | Version of 'serialize'' function that includes version when serializing a value.
dbSerializeValue :: (HasCoreConfiguration, Bi a) => a -> ByteString
dbSerializeValue = serialize' . (dbSerializeVersion,)

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
            either (onParseError key Nothing) pure . decodeFull' $ key
        ToDecodeValue key val ->
            either (onParseError key (Just val)) pure . decodeFull' $ val
  where
    onParseError :: ByteString -> Maybe ByteString -> Text -> m a
    onParseError rawKey rawValMaybe errMsg =
        let valueBuilder = maybe "" (bprint (", value = " %shown)) rawValMaybe
        in throwM $ DBMalformed $ sformat fmtMalformed rawKey valueBuilder errMsg
    fmtMalformed =
        "A key or value stored in DB is malformed, key = "%shown%
        builder%
        ", err: "%stext

dbDecodeMaybe :: (Bi v) => ByteString -> Maybe v
dbDecodeMaybe = rightToMaybe . decodeFull'

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
       (HasCoreConfiguration, Bi (IterKey i), Bi (IterValue i), MonadThrow m, DBIteratorClass i)
    => (ByteString, ByteString)
    -> m (Maybe (IterType i))
processIterEntry (key,val)
    | BS.isPrefixOf prefix key = do
        k <- maybeThrow (DBMalformed $ fmt key "key invalid")
                        (dbDecodeMaybeWP @i key)
        (dbVer, v) <- maybeThrow (DBMalformed $ fmt key "value invalid")
                             (dbDecodeMaybe @(Word8, IterValue i) val)
        checkDBVersion dbVer (k ,v)
    | otherwise = pure Nothing
  where
    prefix = iterKeyPrefix @i
    fmt k err =
      sformat
          ("Iterator entry with keyPrefix = "%shown%" is malformed: \
           \key = "%shown%", err: " %string)
           prefix k err

    checkDBVersion :: HasCoreConfiguration => Word8 -> IterType i -> m (Maybe (IterType i))
    checkDBVersion dbV it
        | dbV == dbSerializeVersion = pure (Just it)
        | otherwise = throwM $ DBUnexpectedVersionTag dbSerializeVersion dbV
