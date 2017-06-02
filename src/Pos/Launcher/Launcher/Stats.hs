{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios, related to 'RawRealModeK'.

module Pos.Launcher.Launcher.Stats
       ( -- 'StatsMode' node launcher.
         runNodeStats
       ) where

import           Mockable                   (Production)
import           Network.Transport.Abstract (Transport)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.DHT.Real               (KademliaDHTInstance)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Runner        (runStatsMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (StatsMode)

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
