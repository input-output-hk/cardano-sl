{-# LANGUAGE ExistentialQuantification #-}

-- | Exceptions hierarchy in cardano-sl.

module Pos.Exception
       ( CardanoException (..)
       , cardanoExceptionToException
       , cardanoExceptionFromException

       , CardanoFatalError (..)
       , reportFatalError
       , assertionFailed
       ) where

import           Control.Exception   (Exception (..))
import qualified Data.Text.Buildable
import           Data.Typeable       (cast)
import           Formatting          (bprint, stext, (%))
import           Serokell.Util       (Color (Red), colorize)
import           System.Wlog         (WithLogger, logError)
import qualified Text.Show
import           Universum

-- | Root of exceptions in cardano-sl.
data CardanoException =
    forall e. (Buildable e, Exception e) =>
              CardanoException e
    deriving (Typeable)

instance Show CardanoException where
    show (CardanoException e) = toString . pretty $ e

instance Exception CardanoException

instance Buildable CardanoException where
    build (CardanoException e) = Data.Text.Buildable.build e

-- | Helper to define sub-exception of CardanoException.
cardanoExceptionToException :: (Buildable e, Exception e) => e -> SomeException
cardanoExceptionToException = toException . CardanoException

-- | Helper to define sub-exception of CardanoException.
cardanoExceptionFromException :: Exception e => SomeException -> Maybe e
cardanoExceptionFromException x = do
    CardanoException a <- fromException x
    cast a


-- | Error indicating that something really bad happened. Should be
-- used when serious assertions fail (local equivalent of
-- 'panic'). 'panic' is still alright to use, but preferably in pure
-- environment.
data CardanoFatalError =
    CardanoFatalError !Text
    deriving (Typeable, Show)

instance Buildable CardanoFatalError where
    build (CardanoFatalError msg) =
        bprint ("Cardano fatal error: "%stext) msg

instance Exception CardanoFatalError where
    toException = cardanoExceptionToException
    fromException = cardanoExceptionFromException
    displayException = toString . pretty

-- | Print red message about fatal error and throw exception.
reportFatalError
    :: (WithLogger m, MonadThrow m)
    => Text -> m a
reportFatalError msg = do
    logError $ colorize Red msg
    throwM $ CardanoFatalError msg

-- | Report 'CardanoFatalError' for failed assertions.
assertionFailed :: (WithLogger m, MonadThrow m) => Text -> m a
assertionFailed msg =
    reportFatalError $ "assertion failed: " <> msg
