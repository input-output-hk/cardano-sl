{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes       #-}

module Pos.Util.Monitor (

      startMonitor
    , stopMonitor

    ) where

import           Control.Concurrent (killThread)
import           Control.Monad.IO.Class (MonadIO)
import           Mockable.Class (Mockable)
import qualified Mockable.Metrics as Metrics
import           Node (Node)
import           Node.Util.Monitor (registerMetrics)
import           Pos.System.Metrics.Constants (cardanoNamespace)
import qualified System.Metrics as Monitoring
import qualified System.Metrics.Distribution as Monitoring.Distribution
import qualified System.Remote.Monitoring.Wai as Monitoring

import           Universum

startMonitor
    :: ( Mockable Metrics.Metrics m
       , Metrics.Distribution m ~ Monitoring.Distribution.Distribution
       , MonadIO m
       )
    => Int
    -> (forall t . m t -> IO t)
    -> Node m
    -> m Monitoring.Server
startMonitor port lowerIO node = do
    store <- liftIO Monitoring.newStore
    registerMetrics (Just cardanoNamespace) lowerIO node store
    liftIO $ Monitoring.registerGcMetrics store
    server <- liftIO $ Monitoring.forkServerWith store "127.0.0.1" port
    liftIO . putStrLn $ "Forked EKG server on port " ++ show port
    return server

stopMonitor
    :: ( MonadIO m )
    => Monitoring.Server
    -> m ()
stopMonitor server = liftIO $ killThread (Monitoring.serverThreadId server)
