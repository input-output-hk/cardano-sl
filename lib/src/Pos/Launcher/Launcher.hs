{-# LANGUAGE RankNTypes #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
         runNodeReal
       ) where

import           Mockable (Production)

import           Pos.Communication.Protocol (OutSpecs)
import           Pos.Core (HasConfiguration)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..), bracketNodeResources)
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Worker.Types (WorkerSpec)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => NodeParams
    -> SscParams
    -> ([WorkerSpec (RealMode EmptyMempoolExt)], OutSpecs)
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp txpGlobalSettings initNodeDBs action
  where
    action :: HasConfiguration => NodeResources EmptyMempoolExt -> Production ()
    action nr@NodeResources {..} = runRealMode nr (runNode nr plugins)
