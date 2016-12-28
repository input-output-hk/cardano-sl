{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Util.Iterator
       (
         MonadIterator (..)
       , ListHolder
       , runListHolder
       ) where

import           Control.Monad.State (state)
import           Control.Monad.Trans (MonadTrans)
import           Universum

-- | MonadIterator encapsulates iteration by collection elements of type @a@.
-- Traverse starts with first element of container.
class Monad m => MonadIterator m a where
    -- | Returns the current element of the iteration if it exists
    -- and moves iterator to the next element.
    -- Returns Nothing if the end of collection is reached
    -- or next traverse is impossible.
    nextItem :: m (Maybe a)
    -- | Returns the current element of the iteration if it exists
    -- or Nothing if it doesn't exist.
    curItem  :: m (Maybe a)

    default nextItem :: MonadTrans t => t m (Maybe a)
    nextItem = lift nextItem

    default curItem :: MonadTrans t => t m (Maybe a)
    curItem = lift curItem

instance MonadIterator m a => MonadIterator (StateT s m) a

-- | Encapsulation of list iterator.
newtype ListHolder s a = ListHolder (State [s] a)
    deriving (Functor, Applicative, Monad)

instance MonadIterator (ListHolder a) a where
    nextItem = ListHolder $ state $ \s->
        case s of
            []     -> (Nothing, [])
            (x:xs) -> (Just x, xs)
    curItem = ListHolder $ state $ \s->
        case s of
            []      -> (Nothing, [])
            l@(x:_) -> (Just x, l)

runListHolder :: ListHolder s a -> [s] -> a
runListHolder (ListHolder s) = evalState s
