{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Util.Context
       (
       -- * Context
         ContextPart(..)
       , HasContext
       , HasContexts
       , getContext
       , askContext
       , viewContext
       ) where

import           Universum

import           Control.Lens                     (Getting)
import qualified Control.Monad.Ether.Implicit     as Ether

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
