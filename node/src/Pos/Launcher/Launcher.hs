-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Mockable                   (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Configuration          (HasNodeConfiguration)
import           Pos.Core                   (HasConfiguration)
import           Pos.Infra.Configuration    (HasInfraConfiguration)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Resource      (NodeResources (..), bracketNodeResources)
import           Pos.Launcher.Runner        (runRealMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.Configuration   (HasUpdateConfiguration)
import           Pos.WorkMode               (RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: forall ssc.
       ( SscConstraint ssc
       , HasConfiguration
       , HasUpdateConfiguration
       , HasInfraConfiguration
       -- FIXME avieth
       -- godtossing is always assumed, regardless of the forall ssc.
       -- It wasn't noticed before, because the godtossing data was in some
       -- global mutable variable and so didn't appear as a bona fide dependency
       -- in any constraint or left-hand-side of an arrow.
       -- See 'prepareGStateDB'.
       , HasGtConfiguration
       , HasNodeConfiguration
       )
    => NodeParams
    -> SscParams ssc
    -> ([WorkerSpec (RealMode ssc)], OutSpecs)
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp action
  where
    action :: HasConfiguration => NodeResources ssc (RealMode ssc) -> Production ()
    action nr@NodeResources {..} =
        runRealMode
            nr
            (runNode @ssc nr plugins)
