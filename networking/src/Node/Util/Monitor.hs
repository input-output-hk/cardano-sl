{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE RankNTypes       #-}

module Node.Util.Monitor (
      registerMetrics
    ) where

import           Control.Monad.IO.Class
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Mockable.Class
import qualified Mockable.Metrics as Metrics
import           Node
import qualified System.Metrics as Monitoring
import qualified System.Metrics.Distribution as Monitoring.Distribution

-- | Put time-warp related metrics into an EKG store.
--   You must indicate how to run the monad into IO, so that EKG can produce
--   the metrics (it works in IO).
registerMetrics
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , MonadIO m
       )
    => Maybe T.Text
    -> (forall t . m t -> IO t)
    -> Node m
    -> Monitoring.Store
    -> m ()
registerMetrics mbNamespace lowerIO node store = do
    liftIO $ flip (Monitoring.registerGauge (withNamespace "handlers.initiated_remotely")) store $ lowerIO $ do
        stats <- nodeStatistics node
        Metrics.readGauge (stRunningHandlersRemote stats)
    liftIO $ flip (Monitoring.registerGauge (withNamespace "handlers.initiated_locally")) store $ lowerIO $ do
        stats <- nodeStatistics node
        Metrics.readGauge (stRunningHandlersLocal stats)
    liftIO $ flip (Monitoring.registerDistribution (withNamespace "handlers.finished_normally.elapsed_time_microseconds")) store $ lowerIO $ do
        stats <- nodeStatistics node
        liftIO $ Monitoring.Distribution.read (stHandlersFinishedNormally stats)
    liftIO $ flip (Monitoring.registerDistribution (withNamespace "handlers.finished_exceptionally.elapsed_time_microseconds")) store $ lowerIO $ do
        stats <- nodeStatistics node
        liftIO $ Monitoring.Distribution.read (stHandlersFinishedExceptionally stats)
  where
      withNamespace :: T.Text -> T.Text
      withNamespace name = case mbNamespace of
          Nothing -> name
          Just ns -> ns <> "." <> name
