{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
-- | Monad for validation
module Util.Validated (
    Validated(..)
  , validatedFromExceptT
  , validatedFromEither
  , validatedToEither
  , isValidated
    -- * Convenience re-exports
  , MonadError(..)
  ) where

import           Control.Monad.Except
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Universum

-- | Specialization of 'ValidatedT' to 'Identity'
data Validated e a = Invalid e | Valid a

instance Functor (Validated e) where
    fmap = liftM
instance Applicative (Validated e) where
    pure = return
    (<*>) = ap
instance Monad (Validated e) where
    return = Valid
    Invalid e >>= _ = Invalid e
    Valid   a >>= f = f a
instance MonadError e (Validated e) where
    throwError = Invalid
    Invalid e `catchError` f = f e
    Valid   a `catchError` _ = Valid a

validatedFromExceptT :: Monad m => ExceptT e m a -> m (Validated e a)
validatedFromExceptT = fmap validatedFromEither . runExceptT

validatedFromEither :: Either e a -> Validated e a
validatedFromEither (Left  e) = Invalid e
validatedFromEither (Right a) = Valid   a

validatedToEither :: Validated e a -> Either e a
validatedToEither (Invalid e) = Left  e
validatedToEither (Valid   a) = Right a

instance (Buildable e, Buildable a) => Buildable (Validated e a) where
  build (Invalid e) = bprint ("Invalid " % build) e
  build (Valid   a) = bprint ("Valid "   % build) a

isValidated :: Validated e a -> Bool
isValidated (Invalid _) = False
isValidated (Valid   _) = True
