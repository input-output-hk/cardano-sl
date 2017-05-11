{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeProduction
       , runNodeStats
       ) where

import           Universum
import           Mockable                   (Production)
import           Network.Transport.Abstract (Transport)

import           Pos.Communication          (PeerId)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.DHT.Real               (KademliaDHTInstance)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Runner        (runProductionMode, runStatsMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       ( SscConstraint ssc )
    => PeerId
    -> Transport (ProductionMode ssc)
    -> KademliaDHTInstance
    -> ([WorkerSpec (ProductionMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeProduction peerId transport kinst plugins np sscnp =
    runProductionMode peerId transport kinst np sscnp (runNode @ssc (Just kinst) plugins)

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       ( SscConstraint ssc )
    => PeerId
    -> Transport (StatsMode ssc)
    -> KademliaDHTInstance
    -> ([WorkerSpec (StatsMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStats peerId transport kinst plugins np sscnp =
    runStatsMode peerId transport kinst np sscnp (runNode @ssc (Just kinst) plugins)
