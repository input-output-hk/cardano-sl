{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}

-- | Runners in various modes.

module Pos.Launcher.Runner
       ( -- * High level runners
         runRealMode

       , elimRealMode

       -- * Exported for custom usage in CLI utils
       , runServer
       ) where

import           Universum

import           Control.Concurrent.Async (race)
import qualified Control.Monad.Reader as Mtl
import           Data.Default (Default)
import           System.Exit (ExitCode (..))

import           Pos.Behavior (bcSecurityParams)
import           Pos.Binary ()
import           Pos.Block.Configuration (HasBlockConfiguration,
                     recoveryHeadersMessage, streamWindow)
import           Pos.Configuration (HasNodeConfiguration,
                     networkConnectionTimeout)
import           Pos.Context.Context (NodeContext (..))
import           Pos.Core (StakeholderId, addressHash)
import           Pos.Core.Configuration (HasProtocolConstants,
                     protocolConstants)
import           Pos.Core.JsonLog (jsonLog)
import           Pos.Core.Mockable.Production (Production (..))
import           Pos.Crypto (ProtocolMagic, toPublic)
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..),
                     diffusionLayerFull)
import           Pos.Infra.Diffusion.Types (Diffusion (..), DiffusionLayer (..),
                     hoistDiffusion)
import           Pos.Infra.Network.Types (NetworkConfig (..),
                     topologyRoute53HealthCheckEnabled)
import           Pos.Infra.Reporting.Ekg (EkgNodeMetrics (..),
                     registerEkgMetrics, withEkgServer)
import           Pos.Infra.Reporting.Statsd (withStatsd)
import           Pos.Infra.Shutdown (ShutdownContext, waitForShutdown)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..),
                     NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Logic.Full (logicFull)
import           Pos.Logic.Types (Logic, hoistLogic)
import           Pos.Recovery.Instance ()
import           Pos.Reporting.Production (ProductionReporterParams (..),
                     productionReporter)
import           Pos.Txp (MonadTxpLocal)
import           Pos.Update.Configuration (HasUpdateConfiguration,
                     lastKnownBlockVersion)
import           Pos.Util.CompileInfo (HasCompileInfo, compileInfo)
import           Pos.Util.Trace (wlogTrace)
import           Pos.Web.Server (withRoute53HealthCheckApplication)
import           Pos.WorkMode (RealMode, RealModeContext (..))

----------------------------------------------------------------------------
-- High level runners
----------------------------------------------------------------------------

-- | Run activity in something convertible to 'RealMode' and back.
runRealMode
    :: forall ext a.
       ( Default ext
       , HasCompileInfo
       , HasConfigurations
       , MonadTxpLocal (RealMode ext)
       -- MonadTxpLocal is meh,
       -- we can't remove @ext@ from @RealMode@ because
       -- explorer and wallet use RealMode,
       -- though they should use only @RealModeContext@
       )
    => ProtocolMagic
    -> NodeResources ext
    -> (Diffusion (RealMode ext) -> RealMode ext a)
    -> IO a
runRealMode pm nr@NodeResources {..} act = runServer
    pm
    ncNodeParams
    (EkgNodeMetrics nrEkgStore)
    ncShutdownContext
    makeLogicIO
    act'
  where
    NodeContext {..} = nrContext
    NodeParams {..} = ncNodeParams
    securityParams = bcSecurityParams npBehaviorConfig
    ourStakeholderId :: StakeholderId
    ourStakeholderId = addressHash (toPublic npSecretKey)
    logic :: Logic (RealMode ext)
    logic = logicFull pm ourStakeholderId securityParams jsonLog
    makeLogicIO :: Diffusion IO -> Logic IO
    makeLogicIO diffusion = hoistLogic (elimRealMode pm nr diffusion) logic
    act' :: Diffusion IO -> IO a
    act' diffusion =
        let diffusion' = hoistDiffusion liftIO (elimRealMode pm nr diffusion) diffusion
         in elimRealMode pm nr diffusion (act diffusion')

-- | RealMode runner: creates a JSON log configuration and uses the
-- resources provided to eliminate the RealMode, yielding a Production (IO).
elimRealMode
    :: forall t ext
     . HasCompileInfo
    => ProtocolMagic
    -> NodeResources ext
    -> Diffusion IO
    -> RealMode ext t
    -> IO t
elimRealMode pm NodeResources {..} diffusion action = runProduction $ do
    Mtl.runReaderT action (rmc nrJsonLogConfig)
  where
    NodeContext {..} = nrContext
    NodeParams {..} = ncNodeParams
    NetworkConfig {..} = ncNetworkConfig
    LoggingParams {..} = bpLoggingParams npBaseParams
    reporterParams = ProductionReporterParams
        { prpServers         = npReportServers
        , prpLoggerConfig    = ncLoggerConfig
        , prpCompileTimeInfo = compileInfo
        , prpTrace           = wlogTrace "reporter"
        , prpProtocolMagic   = pm
        }
    rmc jlConf = RealModeContext
        nrDBs
        nrSscState
        nrTxpState
        nrDlgState
        jlConf
        lpDefaultName
        nrContext
        (productionReporter reporterParams diffusion)

-- | "Batteries-included" server.
-- Bring up a full diffusion layer over a TCP transport and use it to run some
-- action. Also brings up ekg monitoring, route53 health check, statds,
-- according to parameters.
-- Uses magic Data.Reflection configuration for the protocol constants,
-- network connection timeout (nt-tcp), and, and the 'recoveryHeadersMessage'
-- number.
runServer
    :: forall t .
       ( HasProtocolConstants
       , HasBlockConfiguration
       , HasNodeConfiguration
       , HasUpdateConfiguration
       )
    => ProtocolMagic
    -> NodeParams
    -> EkgNodeMetrics
    -> ShutdownContext
    -> (Diffusion IO -> Logic IO)
    -> (Diffusion IO -> IO t)
    -> IO t
runServer pm NodeParams {..} ekgNodeMetrics shdnContext mkLogic act = exitOnShutdown $
    diffusionLayerFull fdconf
                       npNetworkConfig
                       (Just ekgNodeMetrics)
                       mkLogic $ \diffusionLayer -> do
        when npEnableMetrics (registerEkgMetrics ekgStore)
        runDiffusionLayer diffusionLayer $
            maybeWithRoute53 (healthStatus (diffusion diffusionLayer)) $
            maybeWithEkg $
            maybeWithStatsd $
            -- The 'act' is in 'm', and needs a 'Diffusion m'. We can hoist
            -- that, since 'm' is 'MonadIO'.
            (act (diffusion diffusionLayer))

  where
    fdconf = FullDiffusionConfiguration
        { fdcProtocolMagic = pm
        , fdcProtocolConstants = protocolConstants
        , fdcRecoveryHeadersMessage = recoveryHeadersMessage
        , fdcLastKnownBlockVersion = lastKnownBlockVersion
        , fdcConvEstablishTimeout = networkConnectionTimeout
        , fdcTrace = wlogTrace "diffusion"
        , fdcStreamWindow = streamWindow
        }
    exitOnShutdown action = do
        _ <- race (waitForShutdown shdnContext) action
        exitWith (ExitFailure 20) -- special exit code to indicate an update
    ekgStore = enmStore ekgNodeMetrics
    (hcHost, hcPort) = case npRoute53Params of
        Nothing         -> ("127.0.0.1", 3030)
        Just (hst, prt) -> (decodeUtf8 hst, fromIntegral prt)
    maybeWithRoute53 mStatus = case topologyRoute53HealthCheckEnabled (ncTopology npNetworkConfig) of
        True  -> withRoute53HealthCheckApplication mStatus hcHost hcPort
        False -> identity
    maybeWithEkg = case (npEnableMetrics, npEkgParams) of
        (True, Just ekgParams) -> withEkgServer ekgParams ekgStore
        _                      -> identity
    maybeWithStatsd = case (npEnableMetrics, npStatsdParams) of
        (True, Just sdParams) -> withStatsd sdParams ekgStore
        _                     -> identity
