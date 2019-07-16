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


-- The cardano-report-server has been switched off and removing code that
-- depends on it makes building other projects like cardano-byron-proxy
-- easier. We leave the API here, but turn all operations into a NO-OP.

-- | Encapsulates the sending of a report, with potential for side-effects.
newtype Reporter m = Reporter
    { runReporter :: forall a . a -> m ()
    }

noReporter :: Applicative m => Reporter m
noReporter = Reporter (const (pure ()))

-- | Typeclass analgoue of 'Reporter', for those who are allergic to using
-- function arguments.
class Applicative m => MonadReporting m where
    report :: a -> m ()

-- | Report some general information.
reportInfo :: MonadReporting m => Text -> m ()
reportInfo = const $ pure ()

-- | Report «error», i. e. a situation when something is wrong with our
-- node, e. g. an assertion failed.
reportError :: MonadReporting m => Text -> m ()
reportError = const $ pure ()
