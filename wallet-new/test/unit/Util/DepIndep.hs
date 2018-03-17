{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.DepIndep (
    DepIndep(..)
  , liftIndep
    -- ** Convenience re-exports
  , hoist
  ) where

import           Universum
import           Control.Monad.Morph

{-------------------------------------------------------------------------------
  DepIndep monad
-------------------------------------------------------------------------------}

-- | Variation on the reader monad transformer using /two/ environments,
-- where the effects of the underlying monad can depend on the first but not
-- the second environment. In other words, when
--
-- > act :: DepIndep d i m a
--
-- then
--
-- > runDepIndep act d
--
-- returns a pure function which can be executed without incurring more effects.
--
-- This restriction means that @DepIndep d i m@ is /not/ a monad.
--
-- See <https://www.quora.com/Can-you-give-some-exampes-of-Applicative-Functors-which-are-not-Monads>
-- for another example of this kind of "independence restriction".
data DepIndep d i m a = DepIndep { runDepIndep :: d -> m (i -> a) }

instance Functor f => Functor (DepIndep d i f) where
  fmap f (DepIndep g) = DepIndep (fmap (f .) . g)

instance Applicative f => Applicative (DepIndep d i f) where
  pure x                    = DepIndep (\_ -> pure (\_ -> x))
  DepIndep f <*> DepIndep g = DepIndep (\d -> aux <$> f d <*> g d)
    where
      aux :: (i -> a -> b) -> (i -> a) -> (i -> b)
      aux f' g' i = f' i (g' i)

-- | Lift a monadic action that returns a pure function using the
-- " independent " part of the environment (named this way because the monadic
-- effects cannot depend on this environment).
liftIndep :: m (i -> a) -> DepIndep d i m a
liftIndep act = DepIndep (\_ -> act)

-- | The 'MonadTrans' instance is dubious. Yes, we can lift operations from
-- the "base monad" into 'DepIndep', /but/ 'DepIndep' is /not/ a monad.
-- Not sure if we should define it or not.
instance MonadTrans (DepIndep d i) where
  lift act = liftIndep (const <$> act)

instance MFunctor (DepIndep d i) where
  hoist k (DepIndep f) = DepIndep (k . f)

{-------------------------------------------------------------------------------
  Proof that DepIndep cannot be a monad
-------------------------------------------------------------------------------}

-- First, note the difference between 'Applicative' and 'Monad'.
-- In 'Applicative', we can compose effects but the effects cannot depend on
-- values, they must be statically known:
type Ap f = forall a b. f (a -> b) -> f a -> f b

-- By contrast, in a 'Monad' the effects can depend on the arguments:
type Bind m = forall a b. m a -> (a -> m b) -> m b

-- Now, suppose that we had a magical transform that translated a function
-- in which the effects depend on the argument to one in which the effects
-- are determined beforehand:
type Transform m = forall a b. (a -> m b) -> m (a -> b)

-- Then every 'Applicative' would also be a 'Monad':
_applicativeToMonad :: Transform m -> Ap m -> Bind m
_applicativeToMonad t a x f = a (t f) x

-- Now, suppose that we did have a definition of '(>>=)' for 'DepIndep'
-- (for arbitrary types of environments):
type BindDepIndep m = forall d i. Bind (DepIndep d i m)

-- Then we could lift functions of type @d -> i -> m a@ into @DepIndep d i m@:
transformDepIndep :: Applicative m
                  => BindDepIndep m -> (d -> i -> m a) -> DepIndep d i m a
transformDepIndep b f =
    liftIndep (pure identity) `b` \i ->
    DepIndep (\d -> const <$> f d i)

-- Which means we could construct the magical transform that turns every
-- Applicative into a Monad:
_magic :: Applicative m => BindDepIndep m -> Transform m
_magic s f = runDepIndep (transformDepIndep s (\() -> f)) ()
