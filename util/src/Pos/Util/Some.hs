{-# LANGUAGE GADTs      #-}
{-# LANGUAGE RankNTypes #-}

-- | 'Some' data type (existential).

module Pos.Util.Some
       (
         Some(..)
       , Some1(..)
       , applySome
       , liftLensSome
       , liftGetterSome
       ) where

import qualified Prelude
import           Universum

import           Control.Lens (ALens', Getter, Getting, cloneLens, to)

-- | Turn any constraint into an existential type! Example:
--
-- @
-- foo :: Some Show -> String
-- foo (Some s) = show s
-- @
data Some c where
    Some :: c a => a -> Some c

instance Show (Some Show) where
    show (Some s) = show s

-- | Like 'Some', but for @* -> *@ types – for instance, @Some1 Functor ()@
data Some1 c a where
    Some1 :: c f => f a -> Some1 c a

instance Functor (Some1 Functor) where
    fmap f (Some1 x) = Some1 (fmap f x)

-- | Apply a function requiring the constraint
applySome :: (forall a. c a => a -> r) -> (Some c -> r)
applySome f (Some x) = f x

-- | Turn a lens into something operating on 'Some'. Useful for many types
-- like 'HasDifficulty', 'IsHeader', etc.
liftLensSome :: (forall a. c a => ALens' a b)
             -> Lens' (Some c) b
liftLensSome l =
    \f (Some a) -> Some <$> cloneLens l f a

-- | Like 'liftLensSome', but for getters.
liftGetterSome :: (forall a. c a => Getting b a b) -> Getter (Some c) b
liftGetterSome l = \f (Some a) -> Some <$> to (view l) f a
