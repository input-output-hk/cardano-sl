{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Redirects enable instances of some type classes.
-- Here it is related to type classes from 'Pos.DB.Class'.

module Pos.DB.Redirect
       ( dbGetDefault
       , dbIterSourceDefault
       , dbPutDefault
       , dbWriteBatchDefault
       , dbDeleteDefault
       ) where

import           Universum

import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString              as BS (isPrefixOf)
import           Data.Conduit                 (ConduitM, Source, bracketP, yield)
import qualified Database.RocksDB             as Rocks
import           Formatting                   (sformat, shown, string, (%))

import           Pos.Binary.Class             (Bi)
import           Pos.DB.BatchOp               (rocksWriteBatch)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag, IterType,
                                               MonadRealDB, dbTagToLens, getNodeDBs)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (rocksDecodeMaybe, rocksDecodeMaybeWP,
                                               rocksDelete, rocksGetBytes, rocksPutBytes)
import           Pos.DB.Types                 (DB (..))
import           Pos.Util.Util                (maybeThrow)


dbGetDefault :: MonadRealDB m => DBTag -> ByteString -> m (Maybe ByteString)
dbGetDefault tag key = do
    db <- view (dbTagToLens tag) <$> getNodeDBs
    rocksGetBytes key db

dbPutDefault :: MonadRealDB m => DBTag -> ByteString -> ByteString -> m ()
dbPutDefault tag key val = do
    db <- view (dbTagToLens tag) <$> getNodeDBs
    rocksPutBytes key val db

dbWriteBatchDefault :: MonadRealDB m => DBTag -> [Rocks.BatchOp] -> m ()
dbWriteBatchDefault tag batch = do
    db <- view (dbTagToLens tag) <$> getNodeDBs
    rocksWriteBatch batch db

dbDeleteDefault :: MonadRealDB m => DBTag -> ByteString -> m ()
dbDeleteDefault tag key = do
    db <- view (dbTagToLens tag) <$> getNodeDBs
    rocksDelete key db

-- | Conduit source built from rocks iterator.
dbIterSourceDefault ::
       forall m i.
       ( MonadRealDB m
       , MonadResource m
       , DBIteratorClass i
       , Bi (IterKey i)
       , Bi (IterValue i)
       )
    => DBTag
    -> Proxy i
    -> Source m (IterType i)
dbIterSourceDefault tag _ = do
    DB{..} <- view (dbTagToLens tag) <$> lift getNodeDBs
    let createIter = Rocks.createIter rocksDB rocksReadOpts
    let releaseIter i = Rocks.releaseIter i
    let action iter = do
            Rocks.iterSeek iter (iterKeyPrefix @i)
            produce iter
    bracketP createIter releaseIter action
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
    processRes Nothing = pure Nothing
    processRes (Just (key, val))
        | BS.isPrefixOf (iterKeyPrefix @i) key = do
            k <- maybeThrow (DBMalformed $ fmt key "key invalid")
                            (rocksDecodeMaybeWP @i key)
            v <- maybeThrow (DBMalformed $ fmt key "value invalid")
                            (rocksDecodeMaybe val)
            pure $ Just (k, v)
        | otherwise = pure Nothing
    fmt key err =
        sformat
            ("Iterator entry with keyPrefix = "%shown%" is malformed: \
             \key = "%shown%", err: " %string)
            (iterKeyPrefix @i) key err
