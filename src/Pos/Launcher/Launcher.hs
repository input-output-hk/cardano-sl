{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

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

import           Universum

import           Pos.DHT.Real          (KademliaDHTInstance)
import           Pos.Launcher.Param    (NodeParams (..))
import           Pos.Launcher.Runner   (runProductionMode, runStatsMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Security          (SecurityWorkersClass)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Ssc.Class.Types   (SscParams)
import           Pos.WorkMode          (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- Too hard :(
type NodeRunner m = KademliaDHTInstance -> [m ()] -> NodeParams -> IO ()

class NodeRunnerClass ssc m where
    runNodeIO :: KademliaDHTInstance -> [m ()] -> NodeParams -> SscParams ssc -> IO ()

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => KademliaDHTInstance -> [ProductionMode ssc ()] -> NodeParams -> SscParams ssc -> IO ()
runNodeProduction inst plugins np sscnp = runProductionMode inst np sscnp (runNode @ssc plugins)

instance (SscConstraint ssc, SecurityWorkersClass ssc) => NodeRunnerClass ssc (ProductionMode ssc) where
    runNodeIO = runNodeProduction

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => KademliaDHTInstance -> [StatsMode ssc ()] -> NodeParams -> SscParams ssc -> IO ()
runNodeStats inst plugins np sscnp = runStatsMode inst np sscnp (runNode @ssc plugins)

instance (SscConstraint ssc, SecurityWorkersClass ssc) => NodeRunnerClass ssc (StatsMode ssc) where
    runNodeIO = runNodeStats

----------------------------------------------------------------------------
-- Utilities
----------------------------------------------------------------------------
