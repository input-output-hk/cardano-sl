{-# LANGUAGE ConstraintKinds      #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module Pos.Util.Context
       (
       -- * Context
         ExtractContext(..)
       , MonadContext(..)
       , HasContext
       , HasContexts
       , getContext
       , askContext
       , viewContext

       -- * 'ContextT'
       , ContextT(..)
       , runContextT
       , usingContextT

       -- * Multi-contexts
       , ContextSet(..)
       , consContextSet
       ) where

import           Universum

import           Control.Lens        (Getting)
import           Control.Monad.Trans (MonadTrans)

import           Pos.Util.HVect      (HVect)
import qualified Pos.Util.HVect      as HVect

class ExtractContext a s where
    extractContext :: s -> a

class Monad m => MonadContext m where
    type ContextType m
    getFullContext :: m (ContextType m)

    default getFullContext
        :: (MonadTrans t, MonadContext m',
            t m' ~ m, ContextType m' ~ ContextType m)
        => m (ContextType m)
    getFullContext = lift getFullContext

instance MonadContext m => MonadContext (ReaderT s m) where
    type ContextType (ReaderT s m) = ContextType m
instance MonadContext m => MonadContext (StateT s m) where
    type ContextType (StateT s m) = ContextType m
instance MonadContext m => MonadContext (ExceptT s m) where
    type ContextType (ExceptT s m) = ContextType m

{- |
Using 'getFullContext' if not very convenient because usually we don't need
the full context. Thus we define a type synonym 'HasContext' which combines
'MonadContext' and 'ExtractContext' – in particular, @HasContext A m@ means
that inside @m@ you can access context of type @A@.
-}
type HasContext a m = (MonadContext m, ExtractContext a (ContextType m))

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
getContext = extractContext <$> getFullContext
{-# INLINE getContext #-}

-- | Get some field from a context.
askContext :: HasContext a m => (a -> x) -> m x
askContext f = f <$> getContext
{-# INLINE askContext #-}

-- | Get some lens from a context.
viewContext :: HasContext a m => Getting x a x -> m x
viewContext f = view f <$> getContext
{-# INLINE viewContext #-}

{- |
'ContextT' is a specific transformer that can be used for 'MonadContext'.
It's isomorphic to 'ReaderT'.
-}
newtype ContextT context m a = ContextT {getContextT :: ReaderT context m a}
    deriving (Functor, Applicative, Monad)

runContextT :: ContextT context m a -> context -> m a
runContextT act cfg = runReaderT (getContextT act) cfg

usingContextT :: context -> ContextT context m a -> m a
usingContextT cfg act = runReaderT (getContextT act) cfg

{- | @ContextSet '[A, B, C]@ is a collection of contexts from which you can
extract @A@, @B@ or @C@. You're expected to use it instead of creating your
own @Context@ type with subcontexts.

A 'ContextSet' is intended to have only one context of each given type, but
this condition is not checked.
-}
newtype ContextSet (xs :: [*]) = ContextSet (HVect xs)

-- If we know index of a type in a list of types stored in a 'ContextSet', we
-- can extract it from the underlying vector.
instance HVect.Contains x xs => ExtractContext x (ContextSet xs) where
    extractContext (ContextSet v) = HVect.extract v

-- | Add a context to a 'ContextSet'.
consContextSet :: x -> ContextSet xs -> ContextSet (x ': xs)
consContextSet x (ContextSet v) = ContextSet (HVect.cons x v)
