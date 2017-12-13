-- | EKG monitoring.

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}

module Pos.Reporting.Ekg
    ( withEkgServer
    , registerEkgMetrics

    , EkgNodeMetrics (..)
    , registerEkgNodeMetrics
    ) where

import           Universum hiding (bracket)

import           Mockable (Mockable, Bracket, bracket)
import qualified Mockable.Metrics as Mockable
import           Node (Node)
import           Node.Util.Monitor (registerMetrics)

import qualified System.Metrics as Metrics
import qualified System.Metrics.Distribution as Metrics
import qualified System.Metrics.Gauge as Metrics
import qualified System.Metrics.Counter as Metrics
import qualified System.Remote.Monitoring.Wai as Monitoring

import           Pos.Util.Monitor (stopMonitor)
import           Pos.Statistics (EkgParams (..))
import           Pos.System.Metrics.Constants (cardanoNamespace)

-- | All you need in order to register EKG metrics on a time-warp node over
-- 'm'.
data EkgNodeMetrics m = EkgNodeMetrics
    { enmStore :: Metrics.Store
    , enmElim  :: forall t . m t -> IO t
    }

-- | Register various network-related EKG metrics (relevant to a Node).
registerEkgNodeMetrics
    :: ( MonadIO m
       , Mockable Mockable.Metrics m
       , Mockable.Distribution m ~ Metrics.Distribution
       , Mockable.Gauge m ~ Metrics.Gauge
       , Mockable.Counter m ~ Metrics.Counter
       )
    => EkgNodeMetrics m
    -> Node m
    -> m ()
registerEkgNodeMetrics ekgNodeMetrics nd =
    registerMetrics (Just cardanoNamespace) (enmElim ekgNodeMetrics) nd (enmStore ekgNodeMetrics)

-- | Register RTS/GC ekg metrics.
registerEkgMetrics
    :: ( MonadIO m
       )
    => Metrics.Store
    -> m ()
registerEkgMetrics ekgStore = liftIO $ Metrics.registerGcMetrics ekgStore

-- | Bracket an EKG web server, so you can look at the metrics in your browser.
withEkgServer
    :: ( MonadIO m
       , Mockable Bracket m
       )
    => EkgParams
    -> Metrics.Store
    -> m t
    -> m t
withEkgServer EkgParams {..} ekgStore act = bracket acquire release (const act)
  where
    acquire = liftIO $ Monitoring.forkServerWith ekgStore ekgHost ekgPort
    release = stopMonitor
