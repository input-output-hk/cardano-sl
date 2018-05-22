{-# LANGUAGE RankNTypes #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
         runNodeReal
       ) where

import           Universum

-- FIXME we use Production in here only because it gives a 'HasLoggerName'
-- instance so that 'bracketNodeResources' can log.
-- Get rid of production and use a 'Trace IO' instead.
import           Mockable.Production (Production (..))

import           Pos.Core (HasConfiguration)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Diffusion.Types (Diffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..), bracketNodeResources)
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo)
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
    -> [Diffusion (RealMode EmptyMempoolExt) -> RealMode EmptyMempoolExt ()]
    -> IO ()
runNodeReal np sscnp plugins = runProduction $
    bracketNodeResources np sscnp txpGlobalSettings initNodeDBs (Production . action)
  where
    action :: HasConfiguration => NodeResources EmptyMempoolExt -> IO ()
    action nr@NodeResources {..} = runRealMode nr (runNode nr plugins)
