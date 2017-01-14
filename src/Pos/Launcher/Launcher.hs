{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       (
         -- * Node launchers.
         runNodeProduction
       , runNodeStats

         -- * Utility launchers.
       ) where

import           Node                  (SendActions)
import           Mockable              (Production)

import           Pos.Communication.BiP (BiP)
import           Pos.Launcher.Param    (NodeParams (..), BaseParams)
import           Pos.Launcher.Runner   (RealModeResources, runProductionMode,
                                        runStatsMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Ssc.Class         (SscConstraint)
import           Pos.Ssc.Class.Types   (SscParams)
import           Pos.WorkMode          (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => BaseParams
    -> RealModeResources
    -> [SendActions BiP (ProductionMode ssc) -> ProductionMode ssc ()]
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeProduction bp inst plugins np sscnp =
    runProductionMode inst np sscnp (runNode @ssc bp plugins)

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => BaseParams
    -> RealModeResources
    -> [SendActions BiP (StatsMode ssc) -> StatsMode ssc ()]
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStats bp inst plugins np sscnp =
    runStatsMode inst np sscnp (runNode @ssc bp plugins)
