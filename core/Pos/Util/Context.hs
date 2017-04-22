{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}

module Pos.Util.Context
       (
       -- * Context
         ContextTagK(..)
       , ContextPart(..)
       , ContextHolder'
       , HasContext
       , HasContexts
       , getContext
       , askContext
       , viewContext
       ) where

import           Universum

import           Control.Lens                     (Getting)
import qualified Control.Monad.Ether              as Ether.E
import qualified Control.Monad.Ether.Implicit     as Ether
import           Control.Monad.Trans.Ether.Tagged (TaggedTrans (..))
import           Data.Coerce                      (coerce)

data ContextTagK = ContextTag

type ContextHolder' = TaggedTrans 'ContextTag

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

class ContextPart s a where
    contextPart :: Lens' s a

type HasContext a = Ether.MonadReader a

{- |
Declaring that a function needs access to several contexts can be done with
the 'HasContexts' type family:

@
HasContexts [A,B,C] m === (HasContext A m, HasContext B m, HasContext C m)
@
-}
type family HasContexts (xs :: [*]) m :: Constraint where
    HasContexts    '[]    _ = ()
    HasContexts (x ': xs) m = (HasContext x m, HasContexts xs m)

-- | Get a context.
getContext :: HasContext a m => m a
getContext = Ether.ask
{-# INLINE getContext #-}

-- | Get some field from a context.
askContext :: HasContext a m => (a -> x) -> m x
askContext = Ether.asks
{-# INLINE askContext #-}

-- | Get some lens from a context.
viewContext :: HasContext a m => Getting x a x -> m x
viewContext f = Ether.asks (view f)
{-# INLINE viewContext #-}
