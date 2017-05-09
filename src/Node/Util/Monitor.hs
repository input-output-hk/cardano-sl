{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}

module Node.Util.Monitor (

      setupMonitor
    , startMonitor
    , stopMonitor

    ) where

import Control.Monad.IO.Class
import Control.Concurrent (killThread)
import Mockable.Class
import qualified Mockable.Metrics as Metrics
import qualified System.Remote.Monitoring as Monitoring
import qualified System.Metrics as Monitoring
import qualified System.Metrics.Distribution as Monitoring.Distribution
import qualified System.Metrics.Gauge as Monitoring.Gauge
import qualified System.Metrics.Counter as Monitoring.Counter
import Node

-- | Put time-warp related metrics into an EKG store.
--   You must indicate how to run the monad into IO, so that EKG can produce
--   the metrics (it works in IO).
setupMonitor
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , Metrics.Gauge m ~ Monitoring.Gauge.Gauge
       , Metrics.Counter m ~ Monitoring.Counter.Counter
       , MonadIO m
       )
    => (forall t . m t -> IO t)
    -> Node m
    -> Monitoring.Store
    -> m Monitoring.Store
setupMonitor lowerIO node store = do
    liftIO $ flip (Monitoring.registerGauge "Remotely-initated handlers") store $ lowerIO $ do
        stats <- nodeStatistics node
        Metrics.readGauge (stRunningHandlersRemote stats)
    liftIO $ flip (Monitoring.registerGauge "Locally-initated handlers") store $ lowerIO $ do
        stats <- nodeStatistics node
        Metrics.readGauge (stRunningHandlersLocal stats)
    liftIO $ flip (Monitoring.registerDistribution "Handler elapsed time (normal)") store $ lowerIO $ do
        stats <- nodeStatistics node
        liftIO $ Monitoring.Distribution.read (stHandlersFinishedNormally stats)
    liftIO $ flip (Monitoring.registerDistribution "Handler elapsed time (exceptional)") store $ lowerIO $ do
        stats <- nodeStatistics node
        liftIO $ Monitoring.Distribution.read (stHandlersFinishedExceptionally stats)
    return store

startMonitor
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , Metrics.Gauge m ~ Monitoring.Gauge.Gauge
       , Metrics.Counter m ~ Monitoring.Counter.Counter
       , MonadIO m
       )
    => Int
    -> (forall t . m t -> IO t)
    -> Node m
    -> m Monitoring.Server
startMonitor port lowerIO node = do
    store <- liftIO Monitoring.newStore
    store' <- setupMonitor lowerIO node store
    liftIO $ Monitoring.registerGcMetrics store'
    server <- liftIO $ Monitoring.forkServerWith store "127.0.0.1" port
    liftIO . putStrLn $ "Forked EKG server on port " ++ show port
    return server

stopMonitor
    :: ( MonadIO m )
    => Monitoring.Server
    -> m ()
stopMonitor server = liftIO $ killThread (Monitoring.serverThreadId server)
