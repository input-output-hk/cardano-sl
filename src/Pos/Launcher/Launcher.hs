{-# LANGUAGE AllowAmbiguousTypes  #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       (
         -- * Node launchers.
         NodeRunner
       , NodeRunnerClass (runNodeIO)
       , runNodeProduction
       , runNodeStats

         -- * Utility launchers.
       ) where

import           Mockable              (Production)
import           Universum

import           Pos.Launcher.Param    (NodeParams (..))
import           Pos.Launcher.Runner   (RealModeResources, runProductionMode,
                                        runStatsMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.NewDHT.Real       (KademliaDHT (..), KademliaDHTInstance)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Ssc.Class.Types   (SscParams)
import           Pos.WorkMode          (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- Too hard :(
type NodeRunner m = RealModeResources -> [m ()] -> NodeParams -> Production ()

class NodeRunnerClass ssc m where
    runNodeIO :: RealModeResources -> [m ()] -> NodeParams -> SscParams ssc -> Production ()

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => RealModeResources -> [ProductionMode ssc ()] -> NodeParams -> SscParams ssc -> Production ()
runNodeProduction inst plugins np sscnp = runProductionMode inst np sscnp (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (ProductionMode ssc) where
    runNodeIO = runNodeProduction

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => RealModeResources -> [StatsMode ssc ()] -> NodeParams -> SscParams ssc -> Production ()
runNodeStats inst plugins np sscnp = runStatsMode inst np sscnp (runNode @ssc plugins)

instance SscConstraint ssc => NodeRunnerClass ssc (StatsMode ssc) where
    runNodeIO = runNodeStats
