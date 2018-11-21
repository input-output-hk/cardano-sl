{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Different key/value serialization helpers abstracted over
-- 'MonadDB'.

module Pos.DB.Functions
       (
       -- * Encoded putting/getting
         dbGetBi
       , dbPutBi

       -- * Decoding/encoding primitives and iteration related
       , encodeWithKeyPrefix
       , processIterEntry
       ) where

import           Universum

import qualified Data.ByteString as BS (drop, isPrefixOf)
import           Formatting (sformat, shown, string, (%))

import           Pos.Binary.Class (Bi, decodeFull', serialize')
import           Pos.DB.Class (DBIteratorClass (..), DBTag, IterType,
                     MonadDB (..), MonadDBRead (..))
import           Pos.DB.Error (DBError (..))
import           Pos.Util.Util (maybeThrow)

-- | Read serialized value (with version) associated with given key from pure DB.
dbGetBi
    :: forall v m.
       (Bi v, MonadDBRead m)
    => DBTag -> ByteString -> m (Maybe v)
dbGetBi tag key =
    dbGet tag key >>= traverse (either throwM pure . dbDecodeIgnoreVersion)

-- | Write serializable value to DB for given key. Uses simple versioning.
dbPutBi :: (Bi v, MonadDB m) => DBTag -> ByteString -> v -> m ()
dbPutBi tag k v = dbPut tag k (serialize' v)

dbDecodeIgnoreVersion :: forall v . Bi v => ByteString -> Either DBError v
dbDecodeIgnoreVersion bytes = case decodeFull' @v bytes of
    Right val -> Right val
    Left _    -> bimap DBMalformed snd $ decodeFull' @(Word8, v) bytes

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
       (Bi (IterKey i), Bi (IterValue i), MonadThrow m, DBIteratorClass i)
    => (ByteString, ByteString)
    -> m (Maybe (IterType i))
processIterEntry (key,val)
    | BS.isPrefixOf prefix key = do
        k <- maybeThrow (DBMalformed $ fmt key "key invalid")
                        (dbDecodeMaybeWP @i key)
        v <- either throwM pure (dbDecodeIgnoreVersion val)
        pure $ Just (k, v)
    | otherwise = pure Nothing
  where
    prefix = iterKeyPrefix @i
    fmt k err =
      sformat
          ("Iterator entry with keyPrefix = "%shown%" is malformed: \
           \key = "%shown%", err: " %string)
           prefix k err
