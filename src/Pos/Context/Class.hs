{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Class which provides access to NodeContext.

module Pos.Context.Class
       ( WithNodeContext
       , getNodeContext
       , ContextTagK(..)
       ) where

import qualified Control.Monad.Ether              as Ether.E
import           Control.Monad.Trans.Ether.Tagged (TaggedTrans (..))
import           Data.Coerce                      (coerce)
import           Universum

import           Pos.Context.Context              (NodeContext)
import           Pos.Util.Context                 (ContextPart (..))

data ContextTagK = ContextTag

instance (ContextPart ctx x, Monad m, trans ~ ReaderT ctx) =>
  Ether.E.MonadReader x x (TaggedTrans 'ContextTag trans m) where
    ask _ =
      (coerce :: forall a.
        trans m a ->
        Ether.E.ReaderT 'ContextTag ctx m a)
      (view contextPart)
    {-# INLINE ask #-}

    local _ f =
      (coerce :: forall a.
        (trans m a ->
         trans m a) ->
        (Ether.E.ReaderT 'ContextTag ctx m a ->
         Ether.E.ReaderT 'ContextTag ctx m a))
      (local (over contextPart f))
    {-# INLINE local #-}

-- | Class for something that has 'NodeContext' inside.
type WithNodeContext ssc = Ether.E.MonadReader 'ContextTag (NodeContext ssc)

getNodeContext :: WithNodeContext ssc m => m (NodeContext ssc)
getNodeContext = Ether.E.ask (Proxy @'ContextTag)
