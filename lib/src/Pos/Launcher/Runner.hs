{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRealMode
       , runRealBasedMode

       , elimRealMode

       -- * Exported for custom usage in CLI utils
       , runServer
       ) where

import           Universum

import qualified Control.Monad.Reader as Mtl
import           Control.Monad.Fix (MonadFix)
import           Data.Default (Default)
import           Data.Reflection (give)
import           JsonLog (jsonLog)
import           Mockable.Production (Production (..))

import           Pos.Binary ()
import           Pos.Communication (ActionSpec (..), OutSpecs (..))
import           Pos.Communication.Limits (HasAdoptedBlockVersionData)
import           Pos.Context.Context (NodeContext (..))
import           Pos.Core (BlockVersionData)
import           Pos.DB (gsAdoptedBVData)
import           Pos.Diffusion.Types (Diffusion, DiffusionLayer (..))
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..), hoistNodeResources)
import           Pos.Diffusion.Types (DiffusionLayer (..), Diffusion (..))
import           Pos.Diffusion.Full (diffusionLayerFull)
import           Pos.Diffusion.Full.Types (DiffusionWorkMode)
import           Pos.Logic.Full (logicLayerFull, LogicWorkMode)
import           Pos.Logic.Types (LogicLayer (..))
import           Pos.Network.Types (NetworkConfig (..), topologyRoute53HealthCheckEnabled)
import           Pos.Recovery.Instance ()
import           Pos.Reporting.Statsd (withStatsd)
import           Pos.Reporting.Ekg (withEkgServer, registerEkgMetrics, EkgNodeMetrics (..))
import           Pos.Txp (MonadTxpLocal)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog (JsonLogConfig (..), jsonLogConfigFromHandle)
import           Pos.Web.Server (withRoute53HealthCheckApplication)
import           Pos.WorkMode (RealMode, RealModeContext (..), WorkMode)

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in 'RealMode'.
runRealMode
    :: forall ext ctx a.
       (HasCompileInfo, WorkMode ctx (RealMode ext))
    => NodeResources ext (RealMode ext)
    -> (ActionSpec (RealMode ext) a, OutSpecs)
    -> Production a
runRealMode = runRealBasedMode @ext @ctx identity

-- | Run activity in something convertible to 'RealMode' and back.
runRealBasedMode
    :: forall ext ctx m a.
       ( HasCompileInfo
       , WorkMode ctx m
       , Default ext
       , MonadTxpLocal (RealMode ext)
       -- MonadTxpLocal is meh,
       -- we can't remove @ext@ from @RealMode@ because
       -- explorer and wallet use RealMode,
       -- though they should use only @RealModeContext@
       )
    => (forall b. m b -> RealMode ext b)
    -> NodeResources ext m
    -> (ActionSpec (RealMode ext) a, OutSpecs)
    -> Production a
runRealBasedMode unwrap nr@NodeResources {..} (actionSpec, outSpecs) = giveAdoptedBVData $
    elimRealMode hoistedNr $ runServer
        ncNodeParams
        (EkgNodeMetrics nrEkgStore (runProduction . elimRealMode hoistedNr))
        outSpecs
        actionSpec
  where
    hoistedNr = hoistNodeResources unwrap nr
    giveAdoptedBVData :: ((HasAdoptedBlockVersionData (RealMode ext)) => r) -> r
    giveAdoptedBVData = give (gsAdoptedBVData :: RealMode ext BlockVersionData)
    NodeContext {..} = nrContext

-- | RealMode runner: creates a JSON log configuration and uses the
-- resources provided to eliminate the RealMode, yielding a Production (IO).
elimRealMode
    :: forall t ext .
       ( HasConfigurations
       , HasCompileInfo
       , MonadTxpLocal (RealMode ext)
       , HasAdoptedBlockVersionData (RealMode ext)
       )
    => NodeResources ext (RealMode ext)
    -> RealMode ext t
    -> Production t
elimRealMode NodeResources {..} action = do
    jsonLogConfig <- maybe
        (pure JsonLogDisabled)
        jsonLogConfigFromHandle
        nrJLogHandle
    Mtl.runReaderT action (rmc jsonLogConfig)
  where
    NodeContext {..} = nrContext
    NodeParams {..} = ncNodeParams
    NetworkConfig {..} = ncNetworkConfig
    LoggingParams {..} = bpLoggingParams npBaseParams
    rmc jlConf = RealModeContext
        nrDBs
        nrSscState
        nrTxpState
        nrDlgState
        jlConf
        lpDefaultName
        nrContext

-- | "Batteries-included" server.
-- Bring up a full diffusion layer and use it to run some action.
-- Also brings up ekg monitoring, route53 health check, statds, according to
-- parameters. 
runServer
    :: forall ctx m t .
       ( DiffusionWorkMode m
       , LogicWorkMode ctx m
       , MonadFix m
       )
    => NodeParams
    -> EkgNodeMetrics m
    -> OutSpecs
    -> ActionSpec m t
    -> m t
runServer NodeParams {..} ekgNodeMetrics _ (ActionSpec act) =
    logicLayerFull jsonLog $ \logicLayer -> do
        diffusionLayerFull npNetworkConfig (Just ekgNodeMetrics) $ \withLogic -> do
            diffusionLayer <- withLogic (logic logicLayer)
            when npEnableMetrics (registerEkgMetrics ekgStore)
            runLogicLayer logicLayer $
                runDiffusionLayer diffusionLayer $
                maybeWithRoute53 (enmElim ekgNodeMetrics (healthStatus (diffusion diffusionLayer))) $
                maybeWithEkg $
                maybeWithStatsd $
                act (diffusion diffusionLayer)
  where
    ekgStore = enmStore ekgNodeMetrics
    (hcHost, hcPort) = case npRoute53Params of
        Nothing -> ("127.0.0.1", 3030)
        Just (hst, prt) -> (decodeUtf8 hst, fromIntegral prt)
    maybeWithRoute53 mStatus = case topologyRoute53HealthCheckEnabled (ncTopology npNetworkConfig) of
        True -> withRoute53HealthCheckApplication mStatus hcHost hcPort
        False -> identity
    maybeWithEkg = case (npEnableMetrics, npEkgParams) of
        (True, Just ekgParams) -> withEkgServer ekgParams ekgStore
        _ -> identity
    maybeWithStatsd = case (npEnableMetrics, npStatsdParams) of
        (True, Just sdParams) -> withStatsd sdParams ekgStore
        _ -> identity
