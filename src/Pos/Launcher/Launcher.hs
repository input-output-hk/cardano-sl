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

import           Mockable                         (Production)
import           Node                             (SendActions)

import           Pos.Communication.BiP            (BiP)
import           Pos.Communication.Types.Protocol (PeerId)
import           Pos.Launcher.Param               (NodeParams (..))
import           Pos.Launcher.Runner              (RealModeResources, runProductionMode,
                                                   runStatsMode)
import           Pos.Launcher.Scenario            (runNode)
import           Pos.Ssc.Class                    (SscConstraint)
import           Pos.Ssc.Class.Types              (SscParams)
import           Pos.WorkMode                     (ProductionMode, StatsMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeProduction
    :: forall ssc.
       SscConstraint ssc
    => RealModeResources
    -> [ SendActions BiP PeerId (ProductionMode ssc) -> ProductionMode ssc ()]
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeProduction inst plugins np sscnp = runProductionMode inst np sscnp (runNode @ssc plugins)

-- | Run full node in benchmarking node
runNodeStats
    :: forall ssc.
       SscConstraint ssc
    => RealModeResources
    -> [ SendActions BiP PeerId (StatsMode ssc) -> StatsMode ssc ()]
    -> NodeParams
    -> SscParams ssc
    -> Production ()
runNodeStats inst plugins np sscnp = runStatsMode inst np sscnp (runNode @ssc plugins)
