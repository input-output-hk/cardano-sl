{-# LANGUAGE DefaultSignatures     #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Pos.Modern.Iterator
       (
         MonadIterator (..)
       , ListHolder
       , runListHolder
       ) where

import           Control.Monad.State (state)
import           Control.Monad.Trans (MonadTrans)
import           Universum

class Monad m => MonadIterator m a where
    nextItem :: m (Maybe a)
    curItem  :: m (Maybe a)

    default nextItem :: MonadTrans t => t m (Maybe a)
    nextItem = lift nextItem

    default curItem :: MonadTrans t => t m (Maybe a)
    curItem = lift curItem

instance MonadIterator m a => MonadIterator (StateT s m) a

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
