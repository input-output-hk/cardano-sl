{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext (..)
       ) where

import           Control.Monad.Trans          (MonadTrans)
import           Universum

import           Pos.Context.Context          (NodeContext)

-- | Class for something that has 'NodeContext' inside.
class Monad m => WithNodeContext ssc m | m -> ssc where
    getNodeContext :: m (NodeContext ssc)

    default getNodeContext :: (MonadTrans t, WithNodeContext ssc m', t m' ~ m) =>
        m (NodeContext ssc)
    getNodeContext = lift getNodeContext

instance {-# OVERLAPPABLE #-}
  (WithNodeContext ssc m, MonadTrans t, Monad (t m)) =>
  WithNodeContext ssc (t m)
