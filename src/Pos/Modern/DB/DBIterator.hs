{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Pos.Modern.DB.DBIterator
       (
         DBIterator (..)
       , DBMapIterator (..)
       , withIterator
       , mapIterator
       ) where

import           Control.Monad.Reader    (ReaderT (..))
import           Control.Monad.Trans     (MonadTrans)
import qualified Database.RocksDB        as Rocks
import           Universum

import           Pos.Binary.Class        (Bi)
import           Pos.Modern.DB.Functions (rocksDecode)
import           Pos.Modern.DB.Types     (DB (..))
import           Pos.Modern.Iterator     (MonadIterator (..))


newtype DBIterator m a = DBIterator
    { getDBIterator :: ReaderT Rocks.Iterator m a
    } deriving (Functor, Applicative, Monad, MonadIO, MonadTrans, MonadThrow)

instance (Bi k, Bi v, MonadIO m, MonadThrow m)
         => MonadIterator (DBIterator m) (k, v) where
    nextItem = do
        it <- DBIterator ask
        kv <- Rocks.iterEntry it
        Rocks.iterNext it
        case kv of
            Nothing     -> pure Nothing
            Just (k, v) ->
                Just <$> ((,) <$> rocksDecode k <*> rocksDecode v)
    curItem = DBIterator ask >>= \it -> do
        kv <- Rocks.iterEntry it
        case kv of
            Nothing     -> pure Nothing
            Just (k, v) ->
                Just <$> ((,) <$> rocksDecode k <*> rocksDecode v)

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
            (\it -> runReaderT (getDBIterator dbIter) it)

mapIterator :: forall u v m ssc a . (MonadIO m, MonadMask m)
            => DBMapIterator (u->v) m a -> (u->v) -> DB ssc -> m a
mapIterator dbIter f = withIterator (runReaderT (getDBMapIterator dbIter) f)
