{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables  #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext
       , getNodeContext
       , NodeContextTagK(..)
       ) where

import qualified Control.Monad.Ether as Ether.E
import qualified Control.Monad.Trans.Ether.Tagged
import           Universum
import           Data.Coerce (coerce)

import           Pos.Context.Context (NodeContext)
import           Pos.Util.Context (ContextPart(..))

data NodeContextTagK = NodeContextTag

instance (ContextPart ctx x, Monad m) =>
  Ether.E.MonadReader x x (Ether.E.ReaderT 'NodeContextTag ctx m) where
    ask _ =
      (coerce :: forall a.
        ReaderT ctx m a ->
        Ether.E.ReaderT 'NodeContextTag ctx m a)
      (view contextPart)
    {-# INLINE ask #-}

    local _ f =
      (coerce :: forall a.
        (ReaderT ctx m a ->
         ReaderT ctx m a) ->
        (Ether.E.ReaderT 'NodeContextTag ctx m a ->
         Ether.E.ReaderT 'NodeContextTag ctx m a))
      (local (over contextPart f))
    {-# INLINE local #-}

-- | Class for something that has 'NodeContext' inside.
type WithNodeContext ssc = Ether.E.MonadReader 'NodeContextTag (NodeContext ssc)

getNodeContext :: WithNodeContext ssc m => m (NodeContext ssc)
getNodeContext = Ether.E.ask (Proxy @'NodeContextTag)
