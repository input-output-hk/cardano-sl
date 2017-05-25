{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeProduction
       , runNodeStats
       , runNodeStatic
       ) where

import           Data.Set                   (Set)
import           Mockable                   (Production)
import           Network.Transport.Abstract (Transport)

import           Pos.Communication          (NodeId)
import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.DHT.Real               (KademliaDHTInstance)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Runner        (runProductionMode, runStaticMode,
                                             runStatsMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (ProductionMode, StaticMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => Transport (ProductionMode ssc)
    -> KademliaDHTInstance
    -> ([WorkerSpec (ProductionMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeProduction transport kinst plugins np sscnp =
    runProductionMode transport kinst np sscnp (runNode @ssc plugins)

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => Transport (StatsMode ssc)
    -> KademliaDHTInstance
    -> ([WorkerSpec (StatsMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStats transport kinst plugins np sscnp =
    runStatsMode transport kinst np sscnp (runNode @ssc plugins)

-- | Run full node in static mode
runNodeStatic
    :: forall ssc.
       SscConstraint ssc
    => Transport (StaticMode ssc)
    -> Set NodeId
    -> ([WorkerSpec (StaticMode ssc)], OutSpecs)
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStatic transport peers plugins np sscnp =
    runStaticMode transport peers np sscnp (runNode @ssc plugins)
