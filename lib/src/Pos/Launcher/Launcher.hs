{-# LANGUAGE RankNTypes #-}
-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launchers.
         runNodeReal
       ) where

import           Universum

import           Data.Reflection (give)
import           Mockable (Production)

import           Pos.Communication.Protocol (OutSpecs, WorkerSpec)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Communication.Limits (HasAdoptedBlockVersionData)
import           Pos.Core (HasConfiguration, BlockVersionData (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Class (gsAdoptedBVData)
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
    -> (HasAdoptedBlockVersionData (RealMode EmptyMempoolExt) => ([WorkerSpec (RealMode EmptyMempoolExt)], OutSpecs))
    -> Production ()
runNodeReal np sscnp plugins = bracketNodeResources np sscnp txpGlobalSettings initNodeDBs action
  where
    action :: HasConfiguration => NodeResources EmptyMempoolExt (RealMode EmptyMempoolExt) -> Production ()
    action nr@NodeResources {..} = giveAdoptedBVData $
        runRealMode
            nr
            (runNode nr plugins)

    -- Fulfill limits here. It's absolutely the wrong place to do it, but this
    -- will go away soon in favour of diffusion/logic split.
    giveAdoptedBVData :: ((HasAdoptedBlockVersionData (RealMode EmptyMempoolExt)) => r) -> r
    giveAdoptedBVData = give (gsAdoptedBVData :: RealMode EmptyMempoolExt BlockVersionData)
