{-# LANGUAGE AllowAmbiguousTypes #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Reporting.Methods
       ( Reporter (..)

       , noReporter

       , MonadReporting (..)

         -- * Report single event.
       , reportError
       , reportInfo

       -- * Exception handling
       , reportOrLog
       , reportOrLogE
       , reportOrLogW

       ) where

import           Universum

import           Control.Exception (ErrorCall (..), Exception (..))
import           Pos.ReportServer.Report (ReportType (..))
import           System.Wlog (Severity (..), WithLogger, logMessage)

import           Pos.DB.Error (DBError (..))
import           Pos.Exception (CardanoFatalError)
import           Pos.Reporting.MemState ()

-- | Encapsulates the sending of a report, with potential for side-effects.
newtype Reporter m = Reporter
    { runReporter :: ReportType -> m ()
    }

noReporter :: Applicative m => Reporter m
noReporter = Reporter (const (pure ()))

-- | Typeclass analgoue of 'Reporter', for those who are allergic to using
-- function arguments.
class MonadReporting m where
    report :: ReportType -> m ()

-- | Report some general information.
reportInfo :: MonadReporting m => Text -> m ()
reportInfo = report . RInfo

-- | Report «error», i. e. a situation when something is wrong with our
-- node, e. g. an assertion failed.
reportError :: MonadReporting m => Text -> m ()
reportError = report . RError

----------------------------------------------------------------------------
-- Exception handling
----------------------------------------------------------------------------

-- | Exception handler which reports (and logs) an exception or just
-- logs it. It reports only few types of exceptions which definitely
-- deserve attention. Other types are simply logged. It's suitable for
-- long-running workers which want to catch all exceptions and restart
-- after delay. If you are catching all exceptions somewhere, you most
-- likely want to use this handler (and maybe do something else).
--
-- NOTE: it doesn't rethrow an exception. If you are sure you need it,
-- you can rethrow it by yourself.
reportOrLog :: (WithLogger m, MonadReporting m) => Severity -> Text -> SomeException -> m ()
reportOrLog severity prefix exc =
    case tryCast @CardanoFatalError <|> tryCast @ErrorCall <|> tryCast @DBError of
        Just msg -> reportError $ prefix <> msg
        Nothing  -> logMessage severity $ prefix <> pretty exc
  where
    tryCast ::
           forall e. Exception e
        => Maybe Text
    tryCast = toText . displayException <$> fromException @e exc

-- | A version of 'reportOrLog' which uses 'Error' severity.
reportOrLogE :: (WithLogger m, MonadReporting m) => Text -> SomeException -> m ()
reportOrLogE = reportOrLog Error

-- | A version of 'reportOrLog' which uses 'Warning' severity.
reportOrLogW :: (WithLogger m, MonadReporting m) => Text -> SomeException -> m ()
reportOrLogW = reportOrLog Warning
