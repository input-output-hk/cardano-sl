{-# LANGUAGE TypeFamilies #-}

module Pos.DHT.MemState.Class
       ( MonadDhtMem (..)
       ) where

import           Control.Monad.Trans    (MonadTrans)
import           Universum

import           Pos.DHT.MemState.Types (DhtContext)

class Monad m => MonadDhtMem m where
    askDhtMem :: m DhtContext

    default askDhtMem :: (MonadTrans t, MonadDhtMem m', t m' ~ m) =>
       m DhtContext
    askDhtMem = lift askDhtMem

instance MonadDhtMem m => MonadDhtMem (ReaderT s m) where
instance MonadDhtMem m => MonadDhtMem (ExceptT s m) where
instance MonadDhtMem m => MonadDhtMem (StateT s m) where
