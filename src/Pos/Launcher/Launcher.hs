-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Mockable                   (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Core                   (HasCoreConstants)
import           Pos.Launcher.Param         (NodeParams (..))
import           Pos.Launcher.Resource      (NodeResources (..), bracketNodeResources)
import           Pos.Launcher.Runner        (runRealMode)
import           Pos.Launcher.Scenario      (runNode)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Ssc.Class              (SscConstraint)
import           Pos.Ssc.Class.Types        (SscParams)
import           Pos.WorkMode               (RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: forall ssc.
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , HasCoreConstants)
    => NodeParams
    -> SscParams ssc
    -> ([WorkerSpec (RealMode ssc)], OutSpecs)
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp action
  where
    action :: HasCoreConstants => NodeResources ssc (RealMode ssc) -> Production ()
    action nr@NodeResources {..} =
        runRealMode
            nr
            (runNode @ssc nr plugins)
