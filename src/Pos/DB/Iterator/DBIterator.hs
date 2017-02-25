{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Encapsulation of RocksDB iterator
module Pos.DB.Iterator.DBIterator
       ( DBIterator (..)
       , DBMapIterator (..)
       , DBKeyIterator
       , DBValueIterator

       , DBnIterator
       , DBnMapIterator
       , runDBnIterator
       , runDBnMapIterator
       , runIterator
       , runMapIterator
       ) where

import qualified Control.Exception.Base       as CE (bracket)
import           Control.Monad.Reader         (ReaderT (..))
import           Control.Monad.Trans          (MonadTrans)
import qualified Data.ByteString              as BS (isPrefixOf)
import qualified Database.RocksDB             as Rocks
import           Formatting                   (sformat, shown, string, (%))
import           Universum

import           Pos.Binary.Class             (Bi)
import           Pos.DB.Class                 (MonadDB (..))
import           Pos.DB.Error                 (DBError (DBMalformed))
import           Pos.DB.Functions             (rocksDecodeMaybe, rocksDecodeMaybeWP)
import           Pos.DB.Holder                (DBHolder (..), runDBHolder)
import           Pos.DB.Iterator.Class        (DBIteratorClass (..), IterType)
import           Pos.DB.Types                 (DB (..), NodeDBs (..))
import           Pos.Util                     (maybeThrow)
import           Pos.Util.Iterator            (MonadIterator (..))

----------------------------------------------------------------------------
-- DBIterator
----------------------------------------------------------------------------

newtype DBIterator i m a = DBIterator
    { getDBIterator :: ReaderT Rocks.Iterator m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans
               , MonadThrow, MonadCatch)

deriving instance MonadMask m => MonadMask (DBIterator i m)
deriving instance MonadDB ssc m => MonadDB ssc (DBIterator i m)

-- | Iterator by keys of type @k@ and values of type @v@.
instance ( Bi k, Bi v
         , MonadIO m, MonadThrow m
         , DBIteratorClass i, IterKey i ~ k, IterValue i ~ v)
         => MonadIterator (k, v) (DBIterator i m) where
    nextItem = do
        it <- DBIterator ask
        cur <- curItem
        Rocks.iterNext it
        return cur
    -- curItem returns first successfully fetched and parsed elements.
    curItem = do
        it <- DBIterator ask
        entryStr <- Rocks.iterEntry it
        if | Nothing <- entryStr -> pure Nothing -- end of Database is reached
           | Just (key, val) <- entryStr,
             BS.isPrefixOf (iterKeyPrefix @i Proxy) key ->
               Just <$> ((,) <$>
                  maybeThrow
                      (DBMalformed $ fmt key "key invalid")
                      (rocksDecodeMaybeWP @i key) <*>
                  maybeThrow
                      (DBMalformed $ fmt key "value invalid")
                      (rocksDecodeMaybe val))
            -- all entries with specified prefix have been viewed
            | otherwise -> pure Nothing
      where
        fmt key err =
          sformat ("Iterator entry with keyPrefix = "%shown%" is malformed: \
                   \key = "%shown%", err: "%string)
                  (iterKeyPrefix @i Proxy) key err

-- | Run DBIterator by `DB ssc`.
runIterator :: forall i a m ssc . (MonadIO m, MonadMask m, DBIteratorClass i)
             => DBIterator i m a -> DB ssc -> m a
runIterator dbIter DB {..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) Rocks.releaseIter run
  where
    run it = do
        Rocks.iterSeek it (iterKeyPrefix @i Proxy)
        runReaderT (getDBIterator dbIter) it

----------------------------------------------------------------------------
-- DBMapIterator
----------------------------------------------------------------------------

-- | Encapsulate `map f elements`, where @elements@ - collection elements of type @a@.
-- Holds `DBIterator m a` and apply f for every `nextItem` and `curItem` call.
-- If f :: a -> b then we iterate by collection elements of type b.
newtype DBMapIterator i v m a = DBMapIterator
    { getDBMapIterator :: ReaderT (IterType i -> v) (DBIterator i m) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

type DBKeyIterator   i = DBMapIterator i (IterKey i)
type DBValueIterator i = DBMapIterator i (IterValue i)

instance MonadTrans (DBMapIterator i v)  where
    lift x = DBMapIterator $ ReaderT $ const $ lift x

-- -- | Instance for DBMapIterator using DBIterator.
-- -- Fetch every element from DBIterator and apply `f` for it.
instance ( Monad m
         , MonadIterator (IterKey i, IterValue i) (DBIterator i m))
         => MonadIterator v (DBMapIterator i v m) where
    nextItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> nextItem
    curItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> curItem

deriving instance MonadMask m => MonadMask (DBMapIterator i v m)
deriving instance MonadDB ssc m => MonadDB ssc (DBMapIterator i v m)
-- | Run DBMapIterator by `DB ssc`.
runMapIterator :: forall i v m ssc a . (MonadIO m, MonadMask m, DBIteratorClass i)
            => DBMapIterator i v m a
            -> (IterType i -> v)
            -> DB ssc
            -> m a
runMapIterator dbIter f = runIterator (runReaderT (getDBMapIterator dbIter) f)

----------------------------------------------------------------------------
-- Wrappers for Iterators in IO
----------------------------------------------------------------------------

-- | Run DBIterator by `DB ssc` in IO.
runIteratorIO :: forall i a m ssc . (MonadIO m, DBIteratorClass i)
            => DBIterator i IO a -> DB ssc -> m a
runIteratorIO dbIter DB {..} = liftIO $
    CE.bracket (Rocks.createIter rocksDB rocksReadOpts) Rocks.releaseIter run
  where
    run it = do
        Rocks.iterSeek it (iterKeyPrefix @i Proxy)
        runReaderT (getDBIterator dbIter) it

-- | Run DBMapIterator by `DB ssc` in IO.
mapIteratorIO :: forall i v m ssc a . (MonadIO m, DBIteratorClass i)
            => DBMapIterator i v IO a
            -> (IterType i -> v)
            -> DB ssc
            -> m a
mapIteratorIO dbIter f = runIteratorIO (runReaderT (getDBMapIterator dbIter) f)

type DBnIterator ssc i      = DBHolder ssc (DBIterator i IO)
type DBnMapIterator ssc i v = DBHolder ssc (DBMapIterator i v IO)

instance MonadIterator e m => MonadIterator e (DBHolder ssc m)

runDBnIterator
    :: forall i m ssc a . (MonadDB ssc m, DBIteratorClass i)
    => (NodeDBs ssc -> DB ssc) -> DBnIterator ssc i a -> m a
runDBnIterator getter dbi = do
    dbs <- getNodeDBs
    let db = getter dbs
    flip (runIteratorIO @i) db $ runDBHolder dbs dbi

runDBnMapIterator
    :: forall i v m ssc a . (MonadDB ssc m, DBIteratorClass i)
    => (NodeDBs ssc -> DB ssc) -> DBnMapIterator ssc i v a -> (IterType i -> v) -> m a
runDBnMapIterator getter dbi f = do
    dbs <- getNodeDBs
    let db = getter dbs
    let it = runDBHolder dbs dbi
    mapIteratorIO @i @v it f db
