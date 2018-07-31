{-# LANGUAGE ExistentialQuantification #-}

-- | Exceptions hierarchy in cardano-sl.

module Pos.Core.Exception
       ( CardanoException (..)
       , cardanoExceptionToException
       , cardanoExceptionFromException

       , CardanoFatalError (..)
       , traceFatalError
       , assertionFailed
       ) where

import           Control.Exception.Safe (Exception (..))
import           Data.Typeable (cast)
import           Formatting (bprint, stext, (%))
import qualified Formatting.Buildable
import           Pos.Util.Trace.Named (TraceNamed, logError)
import           Serokell.Util (Color (Red), colorize)
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
    build (CardanoException e) = Formatting.Buildable.build e

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
traceFatalError
    :: MonadThrow m
    => TraceNamed m -> Text -> m a
traceFatalError tr msg = do
    logError tr (colorize Red msg)
    throwM $ CardanoFatalError msg

-- | Report 'CardanoFatalError' for failed assertions.
assertionFailed :: MonadThrow m => TraceNamed m -> Text -> m a
assertionFailed logTrace msg =
    traceFatalError logTrace $ "assertion failed: " <> msg

