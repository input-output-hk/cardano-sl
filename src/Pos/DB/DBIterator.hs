{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pos.DB.DBIterator
       (
         DBIterator (..)
       , DBMapIterator (..)
       , withIterator
       , mapIterator
       ) where

import           Control.Monad.Reader (ReaderT (..))
import           Control.Monad.Trans  (MonadTrans)
import qualified Database.RocksDB     as Rocks
import           Universum

import           Pos.Binary.Class     (Bi)
import           Pos.DB.Functions     (rocksDecodeKeyValMaybe)
import           Pos.DB.Types         (DB (..))
import           Pos.Modern.Iterator  (MonadIterator (..))


newtype DBIterator m a = DBIterator
    { getDBIterator :: ReaderT Rocks.Iterator m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow)

data ParseResult a = FetchError | DecodeError | Success a

instance (Bi k, Bi v, MonadIO m, MonadThrow m)
         => MonadIterator (DBIterator m) (k, v) where
    nextItem = do
        it <- DBIterator ask
        cur <- curItem
        Rocks.iterNext it
        return cur

    curItem = do
        it <- DBIterator ask
        res <- parseIterator it
        case res of
            FetchError  -> pure Nothing
            DecodeError -> Rocks.iterNext it >> curItem
            Success v   -> pure $ Just v

parseIterator :: (Bi k, Bi v, MonadIO m) => Rocks.Iterator -> m (ParseResult (k, v))
parseIterator it =
    maybe FetchError (maybe DecodeError Success . rocksDecodeKeyValMaybe) <$>
    Rocks.iterEntry it

newtype DBMapIterator f m a = DBMapIterator
    { getDBMapIterator :: ReaderT f (DBIterator m) a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadThrow)

instance MonadTrans (DBMapIterator f)  where
    lift x = DBMapIterator $ ReaderT $ const $ lift x

instance (Monad m, MonadIterator (DBIterator m) u)
         => MonadIterator (DBMapIterator (u->v) m) v where
    nextItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> nextItem
    curItem = DBMapIterator $ ReaderT $ \f -> fmap f <$> curItem

withIterator :: forall b m ssc . (MonadIO m, MonadMask m)
             => DBIterator m b -> DB ssc -> m b
withIterator dbIter DB{..} =
    bracket (Rocks.createIter rocksDB rocksReadOpts) (Rocks.releaseIter)
            (runReaderT (getDBIterator dbIter))

mapIterator :: forall u v m ssc a . (MonadIO m, MonadMask m)
            => DBMapIterator (u->v) m a -> (u->v) -> DB ssc -> m a
mapIterator dbIter f = withIterator (runReaderT (getDBMapIterator dbIter) f)
