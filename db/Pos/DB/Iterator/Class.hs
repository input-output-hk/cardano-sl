{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeFamilies        #-}

module Pos.DB.Iterator.Class
       (
         DBIteratorClass (..)
       , IterType

       , MonadIterator (..)
       , ListHolder
       , ListHolderT (..)
       , runListHolder
       , runListHolderT
       ) where

import           Universum

import           Control.Monad.Base             (MonadBase)
import           Control.Monad.State            (StateT (..))
import           Control.Monad.Trans            (MonadTrans)
import           Control.Monad.Trans.Lift.Local (LiftLocal (..))

----------------------------------------------------------------------------
-- Iterator object
----------------------------------------------------------------------------

-- | Key-value type family encapsulating the iterator (something we
-- can iterate on) functionality.
class DBIteratorClass i where
    type IterKey   i :: *
    type IterValue i :: *
    iterKeyPrefix :: ByteString

type IterType i = (IterKey i, IterValue i)

----------------------------------------------------------------------------
-- MonadIterator with list implementation
----------------------------------------------------------------------------

-- | MonadIterator encapsulates iteration by collection elements of type @a@.
-- Traverse starts with first element of container.
class Monad m => MonadIterator a m where
    -- | Returns the current element of the iteration if it exists
    -- and moves iterator to the next element.
    -- Returns Nothing if the end of collection is reached
    -- or next traverse is impossible.
    nextItem :: m (Maybe a)
    -- | Returns the current element of the iteration if it exists
    -- or Nothing if it doesn't exist.
    curItem  :: m (Maybe a)

    default nextItem :: (MonadTrans t, MonadIterator a n, t n ~ m) => m (Maybe a)
    nextItem = lift nextItem

    default curItem :: (MonadTrans t, MonadIterator a n, t n ~ m) => m (Maybe a)
    curItem = lift curItem

instance {-# OVERLAPPABLE #-}
    (MonadIterator a m, MonadTrans t, Monad (t m)) =>
        MonadIterator a (t m)

-- | Encapsulation of list iterator.
newtype ListHolderT s m a = ListHolderT (StateT [s] m a)
    deriving (Functor,Applicative,Monad,MonadThrow,MonadIO,MonadCatch,MonadTrans,LiftLocal,MonadBase b)

type ListHolder s a = ListHolderT s Identity a

instance Monad m => MonadIterator a (ListHolderT a m) where
    nextItem = ListHolderT $ StateT $ \s -> pure $
        case s of
            []     -> (Nothing, [])
            (x:xs) -> (Just x, xs)
    curItem = ListHolderT $ StateT $ \s -> pure $
        case s of
            []      -> (Nothing, [])
            l@(x:_) -> (Just x, l)

runListHolderT :: forall s m a . Monad m => ListHolderT s m a -> [s] -> m a
runListHolderT (ListHolderT s) = evalStateT s

runListHolder :: forall s a . ListHolder s a -> [s] -> a
runListHolder (ListHolderT s) = evalState s
