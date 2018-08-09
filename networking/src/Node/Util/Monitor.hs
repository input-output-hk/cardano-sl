{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

module Node.Util.Monitor (
      registerMetrics
    ) where

import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Node
import qualified System.Metrics as Monitoring
import qualified System.Metrics.Gauge as Gauge
import qualified System.Metrics.Distribution as Monitoring.Distribution

-- | Put time-warp related metrics into an EKG store.
--   You must indicate how to run the monad into IO, so that EKG can produce
--   the metrics (it works in IO).
registerMetrics
    :: Maybe T.Text
    -> Node
    -> Monitoring.Store
    -> IO ()
registerMetrics mbNamespace node store = do
    flip (Monitoring.registerGauge (withNamespace "handlers.initiated_remotely")) store $ do
        stats <- nodeStatistics node
        Gauge.read (stRunningHandlersRemote stats)
    flip (Monitoring.registerGauge (withNamespace "handlers.initiated_locally")) store $ do
        stats <- nodeStatistics node
        Gauge.read (stRunningHandlersLocal stats)
    flip (Monitoring.registerDistribution (withNamespace "handlers.finished_normally.elapsed_time_microseconds")) store $ do
        stats <- nodeStatistics node
        Monitoring.Distribution.read (stHandlersFinishedNormally stats)
    flip (Monitoring.registerDistribution (withNamespace "handlers.finished_exceptionally.elapsed_time_microseconds")) store $ do
        stats <- nodeStatistics node
        Monitoring.Distribution.read (stHandlersFinishedExceptionally stats)
  where
      withNamespace :: T.Text -> T.Text
      withNamespace name = case mbNamespace of
          Nothing -> name
          Just ns -> ns <> "." <> name
