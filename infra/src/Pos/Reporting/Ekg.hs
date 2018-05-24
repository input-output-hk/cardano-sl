-- | EKG monitoring.

module Pos.Reporting.Ekg
    ( withEkgServer
    , registerEkgMetrics

    , EkgNodeMetrics (..)
    , registerEkgNodeMetrics
    ) where

import           Universum

import           Node (Node)
import           Node.Util.Monitor (registerMetrics)

import qualified System.Metrics as Metrics
import qualified System.Remote.Monitoring.Wai as Monitoring

import           Pos.Util.Monitor (stopMonitor)
import           Pos.Statistics (EkgParams (..))
import           Pos.System.Metrics.Constants (cardanoNamespace)

-- | All you need in order to register EKG metrics on a time-warp node.
data EkgNodeMetrics = EkgNodeMetrics
    { enmStore :: Metrics.Store
    }

-- | Register various network-related EKG metrics (relevant to a Node).
registerEkgNodeMetrics
    :: EkgNodeMetrics
    -> Node
    -> IO ()
registerEkgNodeMetrics ekgNodeMetrics nd =
    registerMetrics (Just cardanoNamespace) nd (enmStore ekgNodeMetrics)

-- | Register RTS/GC ekg metrics.
registerEkgMetrics
    :: Metrics.Store
    -> IO ()
registerEkgMetrics ekgStore = Metrics.registerGcMetrics ekgStore

-- | Bracket an EKG web server, so you can look at the metrics in your browser.
withEkgServer
    :: EkgParams
    -> Metrics.Store
    -> IO t
    -> IO t
withEkgServer EkgParams {..} ekgStore act = bracket acquire release (const act)
  where
    acquire = Monitoring.forkServerWith ekgStore ekgHost ekgPort
    release = stopMonitor
