{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Encapsulation of RocksDB iterator
module Pos.DB.DBIterator
       (
         DBIterator (..)
       , DBMapIterator (..)
       , runIterator
       , mapIterator
       ) where

import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans  (MonadTrans)
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (Bi)
import           Pos.DB.Functions     (rocksDecodeKeyValMaybe)
import           Pos.DB.Types         (DB (..))
import           Pos.Util.Iterator    (MonadIterator (..))

newtype DBIterator m a = DBIterator
    { getDBIterator :: ReaderT Rocks.Iterator m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow)

-- | RocksDB key value iteration errors.
data ParseResult a = FetchError  -- RocksDB internal error
                                 -- caused by invalid database of smth else (on RocksDB side)
                   | DecodeError -- Parsing error caused by invalid format of data
                                 -- or not expected (key, value) pair.
                                 -- (For example we iterate by utxo (key, value)
                                 -- but encounter balance (key, value))
                   | Success a   -- Element is fetched and decoded successfully.

-- | Iterator by keys of type @k@ and values of type @v@.
instance (Bi k, Bi v, MonadIO m, MonadThrow m)
         => MonadIterator (DBIterator m) (k, v) where
    nextItem = do
        it <- DBIterator ask
        cur <- curItem
        Rocks.iterNext it
        return cur
    -- curItem returns first successfully fetched and parsed elements.
    curItem = do
        it <- DBIterator ask
        res <- parseIterator it
        case res of
            FetchError  -> pure Nothing
            DecodeError -> Rocks.iterNext it >> curItem
            Success v   -> pure $ Just v

-- | Parse key and value under iterator.
parseIterator :: (Bi k, Bi v, MonadIO m) => Rocks.Iterator -> m (ParseResult (k, v))
parseIterator it =
    maybe FetchError (maybe DecodeError Success . rocksDecodeKeyValMaybe) <$>
    Rocks.iterEntry it

-- | Encapsulate `map f elements`, where @elements@ - collection elements of type @a@.
-- Holds `DBIterator m a` and apply f for every `nextItem` and `curItem` call.
-- If f :: a -> b then we iterate by collection elements of type b.
newtype DBMapIterator f m a = DBMapIterator
    { getDBMapIterator :: ReaderT f (DBIterator m) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadTrans (DBMapIterator f)  where
    lift x = DBMapIterator $ ReaderT $ const $ lift x

-- | Instance for DBMapIterator using DBIterator.
-- Fetch every element from DBIterator and apply `f` for it.
instance (Monad m, MonadIterator (DBIterator m) u)
         => MonadIterator (DBMapIterator (u->v) m) v where
    nextItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> nextItem
    curItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> curItem

-- | Run DBIterator by `DB ssc`.
runIterator :: forall b m ssc . (MonadIO m, MonadMask m)
             => DBIterator m b -> DB ssc -> m b
runIterator dbIter DB{..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter)
            (runReaderT (getDBIterator dbIter))

-- | Run DBMapIterator by `DB ssc`.
mapIterator :: forall u v m ssc a . (MonadIO m, MonadMask m)
            => DBMapIterator (u->v) m a -> (u->v) -> DB ssc -> m a
mapIterator dbIter f = runIterator (runReaderT (getDBMapIterator dbIter) f)
