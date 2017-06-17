{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Redirects enable instances of some type classes.
-- Here it is related to type classes from 'Pos.DB.Class'.

module Pos.DB.Redirect
       ( DBPureRedirect
       , runDBPureRedirect
       ) where

import           Universum

import           Control.Exception            (SomeException)
import           Control.Monad.Trans.Identity (IdentityT (..))
import           Control.Monad.Trans.Resource (MonadResource)
import qualified Data.ByteString              as BS (isPrefixOf)
import           Data.Coerce                  (coerce)
import           Data.Conduit                 (ConduitM, Source, bracketP, yield)
import qualified Database.RocksDB             as Rocks
import qualified Ether
import           Formatting                   (sformat, shown, string, (%))

import           Pos.Binary.Class             (Bi)
import           Pos.DB.BatchOp               (rocksWriteBatch)
import           Pos.DB.Class                 (DBIteratorClass (..), DBTag, IterType,
                                               MonadDB (..), MonadDBRead (..),
                                               MonadRealDB, dbTagToLens, getNodeDBs)
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (rocksDelete, rocksGetBytes, rocksPutBytes)
import           Pos.DB.Functions             (rocksDecodeMaybe, rocksDecodeMaybeWP)
import           Pos.DB.Types                 (DB (..))
import           Pos.Util.Util                (maybeThrow)


data DBPureRedirectTag

type DBPureRedirect =
    Ether.TaggedTrans DBPureRedirectTag IdentityT

runDBPureRedirect :: DBPureRedirect m a -> m a
runDBPureRedirect = coerce

instance
    (MonadRealDB m, t ~ IdentityT) =>
        MonadDBRead (Ether.TaggedTrans DBPureRedirectTag t m)
  where
    dbGet tag key = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksGetBytes key db
    dbIterSource tag p = iteratorSource tag p

instance
    (MonadRealDB m, t ~ IdentityT) =>
        MonadDB (Ether.TaggedTrans DBPureRedirectTag t m)
  where
    dbPut tag key val = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksPutBytes key val db
    dbWriteBatch tag batch = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksWriteBatch batch db
    dbDelete tag key = do
        db <- view (dbTagToLens tag) <$> getNodeDBs
        rocksDelete key db

-- | Conduit source built from rocks iterator.
iteratorSource ::
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
iteratorSource tag _ = do
    putText $ ("Iterator source, prefix " <> show (iterKeyPrefix @i))
    DB{..} <- view (dbTagToLens tag) <$> lift getNodeDBs
    let createIter = Rocks.createIter rocksDB rocksReadOpts
    let releaseIter i = Rocks.releaseIter i
    let onExc (e :: SomeException) = do
            putText $ "Exception arised in redirect handler: " <> show e
            throwM e
    let action iter = do
            Rocks.iterSeek iter (iterKeyPrefix @i)
            produce iter `catch` onExc
    bracketP createIter releaseIter $ \i -> action i `catch` onExc
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
