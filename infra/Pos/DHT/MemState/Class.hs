{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

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

instance {-# OVERLAPPABLE #-}
  (MonadDhtMem m, MonadTrans t, Monad (t m)) => MonadDhtMem (t m)
