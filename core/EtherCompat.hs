{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module EtherCompat where

import qualified Control.Monad.Reader as Mtl
import           Ether.Internal       (HasLens (..))
import           Universum

type MonadCtx ctx tag r m = (Mtl.MonadReader ctx m, HasLens tag ctx r)

askCtx :: forall tag r ctx m . MonadCtx ctx tag r m => m r
askCtx = asksCtx @tag identity

asksCtx :: forall tag r ctx m a . MonadCtx ctx tag r m => (r -> a) -> m a
asksCtx f = Mtl.asks (f . view (lensOf @tag @ctx @r))

localCtx :: forall tag r ctx m a . MonadCtx ctx tag r m => (r -> r) -> m a -> m a
localCtx f = Mtl.local (over (lensOf @tag @ctx @r) f)
