-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Mockable                         (Production)

import           Pos.Communication.Protocol       (OutSpecs, WorkerSpec)
import           Pos.Configuration                (HasNodeConfiguration)
import           Pos.Core                         (HasConfiguration)
import           Pos.Infra.Configuration          (HasInfraConfiguration)
import           Pos.Launcher.Param               (NodeParams (..))
import           Pos.Launcher.Resource            (NodeResources (..),
                                                   bracketNodeResources)
import           Pos.Launcher.Runner              (runRealMode)
import           Pos.Launcher.Scenario            (runNode)
import           Pos.Ssc.Class                    (SscConstraint)
import           Pos.Ssc.Class.Types              (SscParams)
import           Pos.Ssc.GodTossing.Configuration (HasGtConfiguration)
import           Pos.Update.Configuration         (HasUpdateConfiguration)
import           Pos.Util.CompileInfo             (HasCompileInfo)
import           Pos.WorkMode                     (EmptyMempoolExt, RealMode)
import           Pos.Ssc.GodTossing.Type          (SscGodTossing)

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
       , HasGtConfiguration
       , HasNodeConfiguration
       , HasCompileInfo
       , ssc ~ SscGodTossing
       )
    => NodeParams
    -> SscParams ssc
    -> ([WorkerSpec (RealMode ssc EmptyMempoolExt)], OutSpecs)
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp action
  where
    action :: HasConfiguration => NodeResources ssc EmptyMempoolExt (RealMode ssc EmptyMempoolExt) -> Production ()
    action nr@NodeResources {..} =
        runRealMode
            nr
            (runNode @ssc nr plugins)
