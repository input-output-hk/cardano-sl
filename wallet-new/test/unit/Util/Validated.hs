{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE PatternSynonyms            #-}
-- | Monad for validation
module Util.Validated (
    Validated(..)
  , validatedFromExceptT
  , validatedFromEither
  , validatedToEither
  , isValidated
  , addErrorDetail
  , validatedMapErrors
    -- * Convenience re-exports
  , MonadError(..)
  ) where

import           Control.Monad.Except
import qualified Data.Text.Buildable
import           Formatting (bprint, build, (%))
import           Serokell.Util (listJson)
import           Universum

-- | Mark a value as validated or not
--
-- If not validated, we can include some arbitrary error message as well
-- as some additional text detail.
data Validated e a = Invalid [Text] e | Valid a

addErrorDetail :: Text -> Validated e a -> Validated e a
addErrorDetail d (Invalid ds e) = Invalid (d:ds) e
addErrorDetail _ (Valid      a) = Valid a

instance Functor (Validated e) where
    fmap = liftM
instance Applicative (Validated e) where
    pure = return
    (<*>) = ap
instance Monad (Validated e) where
    return = Valid
    Invalid ds e >>= _ = Invalid ds e
    Valid      a >>= f = f a
instance MonadError e (Validated e) where
    throwError = Invalid []
    Invalid _ e `catchError` f = f e
    Valid     a `catchError` _ = Valid a

validatedFromExceptT :: Monad m => ExceptT e m a -> m (Validated e a)
validatedFromExceptT = fmap validatedFromEither . runExceptT

validatedFromEither :: Either e a -> Validated e a
validatedFromEither (Left  e) = Invalid [] e
validatedFromEither (Right a) = Valid      a

validatedToEither :: Validated e a -> Either e a
validatedToEither (Invalid _ e) = Left  e
validatedToEither (Valid     a) = Right a

instance (Buildable e, Buildable a) => Buildable (Validated e a) where
  build (Invalid ds e) = bprint ("Invalid " % listJson % " " % build) ds e
  build (Valid      a) = bprint ("Valid "   % build) a

isValidated :: Validated e a -> Bool
isValidated (Invalid _ _) = False
isValidated (Valid     _) = True

validatedMapErrors :: (e -> e') -> Validated e a -> Validated e' a
validatedMapErrors f (Invalid ds e) = Invalid ds (f e)
validatedMapErrors _ (Valid a)      = Valid a
