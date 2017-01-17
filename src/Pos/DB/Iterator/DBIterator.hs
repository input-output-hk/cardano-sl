{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Encapsulation of RocksDB iterator
module Pos.DB.Iterator.DBIterator
       ( DBIterator (..)
       , DBMapIterator (..)
       , DBKeyIterator
       , DBValueIterator
       , runIterator
       , mapIterator
       ) where

import           Control.Monad.Reader  (ReaderT (..))
import           Control.Monad.Trans   (MonadTrans)
import qualified Data.ByteString       as BS (isPrefixOf)
import qualified Database.RocksDB      as Rocks
import           Universum

import           Pos.Binary.Class      (Bi)
import           Pos.DB.Class          (MonadDB (..))
import           Pos.DB.Error          (DBError (DBMalformed))
import           Pos.DB.Functions      (rocksDecodeMaybe, rocksDecodeMaybeWP)
import           Pos.DB.Iterator.Class (DBIteratorClass (..), IterType)
import           Pos.DB.Types          (DB (..))
import           Pos.Util.Iterator     (MonadIterator (..))

newtype DBIterator i m a = DBIterator
    { getDBIterator :: ReaderT Rocks.Iterator m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans
               , MonadThrow, MonadCatch)

-- | RocksDB key value iteration errors.
data ParseResult a = FetchError  -- RocksDB internal error
                                 -- caused by invalid database of smth else (on RocksDB side)
                   | DecodeError -- Parsing error caused by invalid format of data
                                 -- or not expected (key, value) pair.
                                 -- (For example we iterate by utxo (key, value)
                                 -- but encounter balance (key, value))
                   | Success a   -- Element is fetched and decoded successfully.
    deriving Show

-- | Iterator by keys of type @k@ and values of type @v@.
instance ( Bi k, Bi v
         , MonadIO m, MonadThrow m
         , DBIteratorClass i, IterKey i ~ k, IterValue i ~ v,
           Show k, Show v --TODO remove after debug
         )
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
                maybe (throwM $ DBMalformed "Invalid entry") (pure . Just) $
                    (,) <$> rocksDecodeMaybeWP @i key <*> rocksDecodeMaybe val
            | otherwise -> pure Nothing -- all entries with specified prefix was viewed

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

deriving instance MonadMask m => MonadMask (DBIterator i m)
deriving instance MonadMask m => MonadMask (DBMapIterator i v m)

deriving instance MonadDB ssc m => MonadDB ssc (DBIterator i m)
deriving instance MonadDB ssc m => MonadDB ssc (DBMapIterator i v m)

-- | Run DBIterator by `DB ssc`.
runIterator :: forall i a m ssc . (MonadIO m, MonadMask m, DBIteratorClass i)
             => DBIterator i m a -> DB ssc -> m a
runIterator dbIter DB{..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter)
--            (\it -> Rocks.iterFirst it >> runReaderT (getDBIterator dbIter) it)
            (\it -> Rocks.iterSeek it (iterKeyPrefix @i Proxy) >> runReaderT (getDBIterator dbIter) it)

-- | Run DBMapIterator by `DB ssc`.
mapIterator :: forall i v m ssc a . (MonadIO m, MonadMask m, DBIteratorClass i)
            => DBMapIterator i v m a
            -> (IterType i -> v)
            -> DB ssc
            -> m a
mapIterator dbIter f = runIterator (runReaderT (getDBMapIterator dbIter) f)
