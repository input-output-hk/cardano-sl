{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext (..)
       ) where

import           Control.Monad.Except         (ExceptT)
import           Control.Monad.Trans.Resource (ResourceT)
import           Universum

import           Pos.Context.Context          (NodeContext)

-- | Class for something that has 'NodeContext' inside.
class WithNodeContext ssc m | m -> ssc where
    getNodeContext :: m (NodeContext ssc)

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ReaderT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (StateT a m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ExceptT e m) where
    getNodeContext = lift getNodeContext

instance (Monad m, WithNodeContext ssc m) =>
         WithNodeContext ssc (ResourceT m) where
    getNodeContext = lift getNodeContext
