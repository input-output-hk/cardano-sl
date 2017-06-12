{-# LANGUAGE ScopedTypeVariables #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Mockable                   (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Resource      (NodeResources (..), bracketNodeResources,
                                             hoistNodeResources)
import           Pos.Launcher.Runner        (runRealMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.Util.Util              (powerLift)
import           Pos.WorkMode               (RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: forall ssc.
       (SscConstraint ssc, SecurityWorkersClass ssc)
    => NodeParams
    -> SscParams ssc
    -> ([WorkerSpec (RealMode ssc)], OutSpecs)
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp action
  where
    action nr@NodeResources {..} =
        runRealMode
            (hoistNodeResources powerLift nr)
            (runNode @ssc nrContext plugins)
