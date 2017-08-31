{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Node.Util.Monitor (
      setupMonitor
    ) where

import Control.Monad.IO.Class
import Mockable.Class
import qualified Data.Text as T
import Data.Monoid ((<>))
import qualified Mockable.Metrics as Metrics
import qualified System.Metrics as Monitoring
import qualified System.Metrics.Distribution as Monitoring.Distribution
import Node

-- | Put time-warp related metrics into an EKG store.
--   You must indicate how to run the monad into IO, so that EKG can produce
--   the metrics (it works in IO).
setupMonitor
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , MonadIO m
       )
    => Maybe T.Text
    -> (forall t . m t -> IO t)
    -> Node m
    -> Monitoring.Store
    -> m Monitoring.Store
setupMonitor mbNamespace lowerIO node store = do
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
    return store
  where
      withNamespace :: T.Text -> T.Text
      withNamespace name = case mbNamespace of
          Nothing -> name
          Just ns -> ns <> "." <> name
