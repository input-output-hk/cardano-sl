{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
         runNodeRealSimple
       ) where

import           Universum

import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core as Core (Config (..), configBlkSecurityParam)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..),
                     bracketNodeResources)
import           Pos.Launcher.Runner (runRealMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)

-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
-- This function is *only* used by `cardano-node-simple`, the node without
-- wallet functionality.
runNodeRealSimple
    :: (HasConfigurations, HasCompileInfo)
    => Core.Config
    -> TxpConfiguration
    -> NodeParams
    -> SscParams
    -> [  Diffusion (RealMode EmptyMempoolExt)
       -> RealMode EmptyMempoolExt ()
       ]
    -> IO ()
runNodeRealSimple coreConfig txpConfig np sscnp plugins = bracketNodeResources
    (configBlkSecurityParam coreConfig)
    np
    sscnp
    (txpGlobalSettings (configProtocolMagic coreConfig) txpConfig)
    (initNodeDBs coreConfig)
    action
  where
    action :: NodeResources EmptyMempoolExt -> IO ()
    action nr@NodeResources {..} = runRealMode
        coreConfig
        txpConfig
        nr
        (runNode coreConfig txpConfig nr plugins)
