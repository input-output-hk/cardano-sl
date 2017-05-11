{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

-- | Encapsulation of RocksDB iterator
module Pos.DB.Iterator.DBIterator
       ( DBIterator
       , DBMapIterator
       , DBKeyIterator
       , DBValueIterator

       , DBnIterator
       , DBnMapIterator
       , runDBnIterator
       , runDBnMapIterator
       , runIterator
       , runMapIterator
       ) where

import qualified Control.Exception.Base as CE (bracket)
import qualified Data.ByteString        as BS (isPrefixOf)
import qualified Database.RocksDB       as Rocks
import qualified Ether
import           Formatting             (sformat, shown, string, (%))
import           Universum

import           Pos.Binary.Class       (Bi)
import           Pos.DB.Class           (MonadDB, getNodeDBs)
import           Pos.DB.Error           (DBError (DBMalformed))
import           Pos.DB.Functions       (rocksDecodeMaybe, rocksDecodeMaybeWP)
import           Pos.DB.Iterator.Class  (DBIteratorClass (..), IterType)
import           Pos.DB.Types           (DB (..), NodeDBs (..))
import           Pos.Util.Iterator      (MonadIterator (..))
import           Pos.Util.Util          (ether)

----------------------------------------------------------------------------
-- DBIterator
----------------------------------------------------------------------------

data DBIteratorTag i

type DBIterator i = Ether.ReaderT (DBIteratorTag i) Rocks.Iterator

-- | Iterator by keys of type @k@ and values of type @v@.
instance ( Bi k, Bi v
         , MonadIO m, MonadThrow m
         , DBIteratorClass i, IterKey i ~ k, IterValue i ~ v)
         => MonadIterator (k, v) (DBIterator i m) where
    nextItem = do
        it <- ether ask
        cur <- curItem
        Rocks.iterNext it
        return cur
    -- curItem returns first successfully fetched and parsed elements.
    curItem = do
        it <- ether ask
        entryStr <- Rocks.iterEntry it
        if | Nothing <- entryStr -> pure Nothing -- end of Database is reached
           | Just (key, val) <- entryStr,
             BS.isPrefixOf (iterKeyPrefix @i Proxy) key ->
               Just <$> ((,) <$>
                  maybe
                      (throwM $ DBMalformed $ fmt key "key invalid")
                      pure
                      (rocksDecodeMaybeWP @i key) <*>
                  maybe
                      (throwM $ DBMalformed $ fmt key "value invalid")
                      pure
                      (rocksDecodeMaybe val))
            -- all entries with specified prefix have been viewed
            | otherwise -> pure Nothing
      where
        fmt key err =
          sformat ("Iterator entry with keyPrefix = "%shown%" is malformed: \
                   \key = "%shown%", err: "%string)
                  (iterKeyPrefix @i Proxy) key err

-- | Run DBIterator by `DB`.
runIterator
    :: forall i a m.
       (MonadIO m, MonadMask m, DBIteratorClass i)
    => DBIterator i m a -> DB -> m a
runIterator dbIter DB {..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) Rocks.releaseIter run
  where
    run it = do
        Rocks.iterSeek it (iterKeyPrefix @i Proxy)
        Ether.runReaderT dbIter it

----------------------------------------------------------------------------
-- DBMapIterator
----------------------------------------------------------------------------

data DBMapIteratorTag i v

-- | Encapsulate `map f elements`, where @elements@ - collection elements of type @a@.
-- Holds `DBIterator m a` and apply f for every `nextItem` and `curItem` call.
-- If f :: a -> b then we iterate by collection elements of type b.
type DBMapIterator i v =
    Ether.ReaderT
        (DBMapIteratorTag i v)
        (IterType i -> v, Rocks.Iterator)

dbMapIterator
    :: ((IterType i -> v) -> DBIterator i m a)
    -> DBMapIterator i v m a
dbMapIterator mkDbIter =
    Ether.readerT $ \(f, ri) ->
        Ether.runReaderT (mkDbIter f) ri

unDbMapIterator
    :: (IterType i -> v)
    -> DBMapIterator i v m a
    -> DBIterator i m a
unDbMapIterator f dmi =
    Ether.readerT $ \ri ->
        Ether.runReaderT dmi (f, ri)

type DBKeyIterator   i v = DBMapIterator i (IterKey i) v
type DBValueIterator i v = DBMapIterator i (IterValue i) v

-- -- | Instance for DBMapIterator using DBIterator.
-- -- Fetch every element from DBIterator and apply `f` for it.
instance
    ( MonadIterator (IterKey i, IterValue i) (DBIterator i m)
    , p ~ (IterType i -> v, Rocks.Iterator)
    , Monad m ) =>
        MonadIterator v (Ether.ReaderT (DBMapIteratorTag i v) p m)
  where
    nextItem = dbMapIterator (\f -> fmap f <$> nextItem)
    curItem = dbMapIterator (\f -> fmap f <$> curItem)

-- | Run DBMapIterator by `DB`.
runMapIterator
    :: forall i v m a.
       (MonadIO m, MonadMask m, DBIteratorClass i)
    => DBMapIterator i v m a -> (IterType i -> v) -> DB -> m a
runMapIterator dbIter f = runIterator (unDbMapIterator f dbIter)

----------------------------------------------------------------------------
-- Wrappers for Iterators in IO
----------------------------------------------------------------------------

-- | Run DBIterator by `DB` in IO.
runIteratorIO
    :: forall i a m.
       (MonadIO m, DBIteratorClass i)
    => DBIterator i IO a -> DB -> m a
runIteratorIO dbIter DB {..} = liftIO $
    CE.bracket (Rocks.createIter rocksDB rocksReadOpts) Rocks.releaseIter run
  where
    run it = do
        Rocks.iterSeek it (iterKeyPrefix @i Proxy)
        Ether.runReaderT dbIter it

-- | Run DBMapIterator by `DB` in IO.
mapIteratorIO
    :: forall i v m a.
       (MonadIO m, DBIteratorClass i)
    => DBMapIterator i v IO a -> (IterType i -> v) -> DB -> m a
mapIteratorIO dbIter f = runIteratorIO (unDbMapIterator f dbIter)

type DBnIterator i      = Ether.ReaderT' NodeDBs (DBIterator i IO)
type DBnMapIterator i v = Ether.ReaderT' NodeDBs (DBMapIterator i v IO)

runDBnIterator
    :: forall i m a . (MonadDB m, DBIteratorClass i)
    => (NodeDBs -> DB) -> DBnIterator i a -> m a
runDBnIterator getter dbi = do
    dbs <- getNodeDBs
    let db = getter dbs
    flip (runIteratorIO @i) db $ Ether.runReaderT' dbi dbs

runDBnMapIterator
    :: forall i v m a.
       (MonadDB m, DBIteratorClass i)
    => (NodeDBs -> DB) -> DBnMapIterator i v a -> (IterType i -> v) -> m a
runDBnMapIterator getter dbi f = do
    dbs <- getNodeDBs
    let db = getter dbs
    let it = Ether.runReaderT' dbi dbs
    mapIteratorIO @i @v it f db
