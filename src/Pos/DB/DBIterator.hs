{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
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
import           Mockable             (ChannelT, MFunctor' (hoist'),
                                       Mockable (liftMockable), Promise, SharedAtomicT,
                                       ThreadId, bracket, catch, liftMockableWrappedM)
import           Universum            hiding (bracket, catch)

import           Pos.Binary.Class     (Bi)
import           Pos.DB.Class         (MonadDB (..))
import           Pos.DB.Functions     (rocksDecodeKeyValMaybe)
import           Pos.DB.Types         (DB (..))
import           Pos.Util.Iterator    (MonadIterator (..))

newtype DBIterator m a = DBIterator
    { getDBIterator :: ReaderT Rocks.Iterator m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans
               , MonadThrow, MonadCatch)

type instance ThreadId (DBIterator m) = ThreadId m
type instance Promise (DBIterator m) = Promise m
type instance SharedAtomicT (DBIterator m) = SharedAtomicT m
type instance ChannelT (DBIterator m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (ReaderT Rocks.Iterator m) m
         , MFunctor' d (DBIterator m) (ReaderT Rocks.Iterator m)
         ) => Mockable d (DBIterator m) where
    liftMockable = liftMockableWrappedM

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
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow, MonadCatch)

instance MonadTrans (DBMapIterator f)  where
    lift x = DBMapIterator $ ReaderT $ const $ lift x

type instance ThreadId (DBMapIterator f m) = ThreadId m
type instance Promise (DBMapIterator f m) = Promise m
type instance SharedAtomicT (DBMapIterator f m) = SharedAtomicT m
type instance ChannelT (DBMapIterator f m) = ChannelT m

instance ( Mockable d m
         , MFunctor' d (DBMapIterator f m) (ReaderT f (DBIterator m))
         , MFunctor' d (ReaderT f (DBIterator m)) (DBIterator m)
         , MFunctor' d (ReaderT Rocks.Iterator m) m
         , MFunctor' d (DBIterator m) (ReaderT Rocks.Iterator m)
         ) => Mockable d (DBMapIterator f m) where
    liftMockable = liftMockableWrappedM

-- | Instance for DBMapIterator using DBIterator.
-- Fetch every element from DBIterator and apply `f` for it.
instance (Monad m, MonadIterator (DBIterator m) u)
         => MonadIterator (DBMapIterator (u->v) m) v where
    nextItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> nextItem
    curItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> curItem

deriving instance MonadMask m => MonadMask (DBIterator m)
deriving instance MonadMask m => MonadMask (DBMapIterator f m)

deriving instance MonadDB ssc m => MonadDB ssc (DBIterator m)
deriving instance MonadDB ssc m => MonadDB ssc (DBMapIterator f m)

-- | Run DBIterator by `DB ssc`.
runIterator :: forall b m ssc . (MonadIO m, MonadMask m)
             => DBIterator m b -> DB ssc -> m b
runIterator dbIter DB{..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter)
            (\it -> Rocks.iterFirst it >> runReaderT (getDBIterator dbIter) it)

-- | Run DBMapIterator by `DB ssc`.
mapIterator :: forall u v m ssc a . (MonadIO m, MonadMask m)
            => DBMapIterator (u->v) m a -> (u->v) -> DB ssc -> m a
mapIterator dbIter f = runIterator (runReaderT (getDBMapIterator dbIter) f)
