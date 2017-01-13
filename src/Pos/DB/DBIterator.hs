{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

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
import           Pos.DB.Class         (MonadDB (..))
import           Pos.DB.Functions     (WithKeyPrefix (..), rocksDecodeMaybe,
                                       rocksDecodeMaybeWP)
import           Pos.DB.Types         (DB (..))
import           Pos.Util.Iterator    (MonadIterator (..))

newtype DBIterator v m a = DBIterator
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
instance (Bi k, Bi v, MonadIO m, MonadThrow m, WithKeyPrefix k, Show k, Show v)
         => MonadIterator (DBIterator (k, v) m) (k, v) where
    nextItem = do
        it <- DBIterator ask
        cur <- curItem
        Rocks.iterNext it
        return cur
    -- curItem returns first successfully fetched and parsed elements.
    curItem = do
        it <- DBIterator ask
        resk <- parseKey it
        case resk of
            FetchError  -> pure Nothing
            DecodeError -> Rocks.iterNext it >> curItem
            Success k   -> do
                resv <- parseVal it
                case resv of
                    FetchError  -> pure Nothing
                    DecodeError -> Rocks.iterNext it >> curItem
                    Success v   -> pure $ Just (k, v)
      where
        parseKey it =
            maybe FetchError (maybe DecodeError Success . rocksDecodeMaybeWP)
            <$> Rocks.iterKey it
        parseVal it =
            maybe FetchError (maybe DecodeError Success . rocksDecodeMaybe)
            <$> Rocks.iterValue it

-- | Encapsulate `map f elements`, where @elements@ - collection elements of type @a@.
-- Holds `DBIterator m a` and apply f for every `nextItem` and `curItem` call.
-- If f :: a -> b then we iterate by collection elements of type b.
newtype DBMapIterator u v m a = DBMapIterator
    { getDBMapIterator :: ReaderT (u->v) (DBIterator u m) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans (DBMapIterator u v)  where
    lift x = DBMapIterator $ ReaderT $ const $ lift x

-- | Instance for DBMapIterator using DBIterator.
-- Fetch every element from DBIterator and apply `f` for it.
instance (Monad m, MonadIterator (DBIterator u m) u)
         => MonadIterator (DBMapIterator u v m) v where
    nextItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> nextItem
    curItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> curItem

deriving instance MonadMask m => MonadMask (DBIterator u m)
deriving instance MonadMask m => MonadMask (DBMapIterator u v m)

deriving instance MonadDB ssc m => MonadDB ssc (DBIterator u m)
deriving instance MonadDB ssc m => MonadDB ssc (DBMapIterator u v m)

-- | Run DBIterator by `DB ssc`.
runIterator :: forall v a m ssc . (MonadIO m, MonadMask m)
             => DBIterator v m a -> DB ssc -> m a
runIterator dbIter DB{..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter)
            (\it -> Rocks.iterFirst it >> runReaderT (getDBIterator dbIter) it)

-- | Run DBMapIterator by `DB ssc`.
mapIterator :: forall u v m ssc a . (MonadIO m, MonadMask m)
            => DBMapIterator u v m a -> (u->v) -> DB ssc -> m a
mapIterator dbIter f = runIterator (runReaderT (getDBMapIterator dbIter) f)
