{-# LANGUAGE RankNTypes #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
         runNodeReal
       ) where

import           Universum

import           Pos.Core.Configuration (epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..),
                     bracketNodeResources)
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
    => ProtocolMagic
    -> NodeParams
    -> SscParams
    -> [Diffusion (RealMode EmptyMempoolExt) -> RealMode EmptyMempoolExt ()]
    -> IO ()
runNodeReal pm np sscnp plugins =
    bracketNodeResources np sscnp (txpGlobalSettings pm) (initNodeDBs pm epochSlots) action
  where
    action :: NodeResources EmptyMempoolExt -> IO ()
    action nr@NodeResources {..} = runRealMode pm nr (runNode pm nr plugins)
