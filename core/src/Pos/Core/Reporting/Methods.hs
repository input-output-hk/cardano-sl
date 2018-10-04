{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

-- | Methods of reporting different unhealthy behaviour to server.

module Pos.Core.Reporting.Methods
       ( Reporter (..)

       , noReporter

       , MonadReporting (..)

         -- * Report single event.
       , reportError
       , reportInfo

       ) where

import           Universum

import           Pos.ReportServer.Report (ReportType (..))


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
