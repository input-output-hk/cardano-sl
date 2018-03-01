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

import           Control.Monad.Fix (MonadFix)
import qualified Control.Monad.Reader as Mtl
import           Data.Default (Default)
import           JsonLog (jsonLog)
import           Mockable (race)
import           Mockable.Production (Production (..))
import           System.Exit (ExitCode (..))
import           System.Wlog (askLoggerName)

import           Pos.Binary ()
import           Pos.Block.Configuration (HasBlockConfiguration, recoveryHeadersMessage)
import           Pos.Communication (ActionSpec (..), OutSpecs (..))
import           Pos.Configuration (HasNodeConfiguration, networkConnectionTimeout)
import           Pos.Context.Context (NodeContext (..))
import           Pos.Core.Configuration (HasProtocolConstants, protocolConstants)
import           Pos.Crypto.Configuration (HasProtocolMagic, protocolMagic)
import           Pos.Diffusion.Full (FullDiffusionConfiguration (..), diffusionLayerFull)
import           Pos.Diffusion.Types (Diffusion (..), DiffusionLayer (..), hoistDiffusion)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Launcher.Param (BaseParams (..), LoggingParams (..), NodeParams (..))
import           Pos.Launcher.Resource (NodeResources (..))
import           Pos.Logic.Full (LogicWorkMode, logicLayerFull)
import           Pos.Logic.Types (LogicLayer (..), hoistLogic)
import           Pos.Network.Types (NetworkConfig (..), topologyRoute53HealthCheckEnabled)
import           Pos.Recovery.Instance ()
import           Pos.Reporting.Ekg (EkgNodeMetrics (..), registerEkgMetrics, withEkgServer)
import           Pos.Reporting.Statsd (withStatsd)
import           Pos.Shutdown (HasShutdownContext, waitForShutdown)
import           Pos.Txp (MonadTxpLocal)
import           Pos.Update.Configuration (lastKnownBlockVersion)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.JsonLog.Events (JsonLogConfig (..),
                                          jsonLogConfigFromHandle)
import           Pos.Web.Server (withRoute53HealthCheckApplication)
import           Pos.WorkMode (RealMode, RealModeContext (..))
import           Pos.Util.Trace (wlogTrace)

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
    => NodeResources ext
    -> (ActionSpec (RealMode ext) a, OutSpecs)
    -> Production a
runRealMode nr@NodeResources {..} (actionSpec, outSpecs) =
    elimRealMode nr $ runServer
        (runProduction . elimRealMode nr)
        ncNodeParams
        (EkgNodeMetrics nrEkgStore)
        outSpecs
        actionSpec
  where
    NodeContext {..} = nrContext

-- | RealMode runner: creates a JSON log configuration and uses the
-- resources provided to eliminate the RealMode, yielding a Production (IO).
elimRealMode
    :: forall t ext .
       ( HasConfigurations
       , HasCompileInfo
       , MonadTxpLocal (RealMode ext)
       )
    => NodeResources ext
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
-- Bring up a full diffusion layer over a TCP transport and use it to run some
-- action. Also brings up ekg monitoring, route53 health check, statds,
-- according to parameters.
-- Uses magic Data.Reflection configuration for the protocol constants,
-- network connection timeout (nt-tcp), and, and the 'recoveryHeadersMessage'
-- number.
runServer
    :: forall ctx m t .
       ( LogicWorkMode ctx m
       , HasShutdownContext ctx
       , MonadFix m
       , HasProtocolMagic
       , HasProtocolConstants
       , HasBlockConfiguration
       , HasNodeConfiguration
       )
    => (forall y . m y -> IO y)
       -- ^ MonadIO is up in that constraint somewhere. So basically your 'm'
       -- is a reader or IO itself.
    -> NodeParams
    -> EkgNodeMetrics
    -> OutSpecs
    -> ActionSpec m t
    -> m t
runServer runIO NodeParams {..} ekgNodeMetrics _ (ActionSpec act) = do
    lname <- askLoggerName
    exitOnShutdown . logicLayerFull jsonLog $ \logicLayer ->
        liftIO $ diffusionLayerFull (fdconf lname) npNetworkConfig (Just ekgNodeMetrics) (hoistLogic runIO (logic logicLayer)) $ \diffusionLayer -> do
            when npEnableMetrics (registerEkgMetrics ekgStore)
            runIO $ runLogicLayer logicLayer $ liftIO $
                runDiffusionLayer diffusionLayer $
                maybeWithRoute53 (healthStatus (diffusion diffusionLayer)) $
                maybeWithEkg $
                maybeWithStatsd $
                runIO (act (hoistDiffusion liftIO (diffusion diffusionLayer)))
                -- Whew that's a lot of lifting
  where
    fdconf lname = FullDiffusionConfiguration
        { fdcProtocolMagic = protocolMagic
        , fdcProtocolConstants = protocolConstants
        , fdcRecoveryHeadersMessage = recoveryHeadersMessage
        , fdcLastKnownBlockVersion = lastKnownBlockVersion
        , fdcConvEstablishTimeout = networkConnectionTimeout
        , fdcTrace = wlogTrace (lname <> "diffusion")
        }
    exitOnShutdown action = do
        _ <- race waitForShutdown action
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
