-- | Statsd reporting.

module Pos.Reporting.Statsd
    ( withStatsd
    ) where

import           Universum

import           Control.Concurrent (killThread)

import qualified System.Remote.Monitoring.Statsd as Monitoring
import qualified System.Metrics as Metrics
import           Pos.Statistics (StatsdParams (..))

withStatsd
    :: StatsdParams
    -> Metrics.Store
    -> IO t
    -> IO t
withStatsd StatsdParams {..} ekgStore act = bracket acquire release (const act)
  where
    acquire = liftIO $ Monitoring.forkStatsd statsdOptions ekgStore
    release = liftIO . killThread . Monitoring.statsdThreadId
    statsdOptions = Monitoring.defaultStatsdOptions
        { Monitoring.host = statsdHost
        , Monitoring.port = statsdPort
        , Monitoring.flushInterval = statsdInterval
        , Monitoring.debug = statsdDebug
        , Monitoring.prefix = statsdPrefix
        , Monitoring.suffix = statsdSuffix
        }
