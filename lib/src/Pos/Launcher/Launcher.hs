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
import qualified Pos.Util.Log as Log
import           Pos.Util.Trace (natTrace)
import           Pos.Util.Trace.Named (TraceNamed)
import           Pos.WorkMode (EmptyMempoolExt, RealMode)


-----------------------------------------------------------------------------
-- Main launchers
-----------------------------------------------------------------------------

-- | Run full node in real mode.
runNodeReal
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Log.LoggingHandler
    -> TraceNamed IO
    -> ProtocolMagic
    -> NodeParams
    -> SscParams
    -> [Diffusion (RealMode EmptyMempoolExt) -> RealMode EmptyMempoolExt ()]
    -> Production ()
runNodeReal lh logTrace pm np sscnp plugins = --Log.usingLoggerName lh "runNodeReal" $ runProduction $
    --TODO appendName "runNodeReal" to Trace
    bracketNodeResources (natTrace liftIO logTrace) np sscnp (txpGlobalSettings pm) (initNodeDBs pm epochSlots)
        action
  where
    action :: NodeResources EmptyMempoolExt -> Production ()
    action nr@NodeResources {..} =
      runRealMode lh logTrace pm nr (runNode (natTrace liftIO logTrace) pm nr plugins)
