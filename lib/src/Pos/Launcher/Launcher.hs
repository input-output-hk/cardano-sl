{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
    ( -- * Node launcher.
      launchNode

     -- * Actions
    , actionWithCoreNode
    ) where

import           Data.Functor.Contravariant (contramap)
import           Universum

import           Ntp.Client (NtpConfiguration)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (configurationOptions)
import           Pos.Client.CLI.Params (getNodeParams)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp.Logic (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion (..), hoistDiffusion)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     HasConfigurations, WalletConfiguration, cfoKey,
                     withConfigurations)
import           Pos.Launcher.Param (LoggingParams (..), NodeParams (..))
import           Pos.Launcher.Resource (NodeResources, bracketNodeResources,
                     loggerBracket)
import           Pos.Launcher.Runner (elimRealMode, runRealMode)
import           Pos.Launcher.Scenario (runNode)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Trace (Trace, fromTypeclassWlog)
import           Pos.Util.Util (logException)
import           Pos.Util.Wlog (Severity (Info), logInfo)
import           Pos.Worker.Update (updateTriggerWorker)
import           Pos.WorkMode (EmptyMempoolExt)
import           UnliftIO.Async (concurrently_)


-- | Run a given action from a bunch of static arguments
launchNode
    :: NodeArgs
    -> CommonNodeArgs
    -> LoggingParams
    -> (  HasConfigurations
       => Genesis.Config
       -> WalletConfiguration
       -> TxpConfiguration
       -> NtpConfiguration
       -> NodeParams
       -> SscParams
       -> NodeResources EmptyMempoolExt
       -> IO ()
       )
    -> IO ()
launchNode nArgs cArgs lArgs action = do
    let confOpts = configurationOptions (commonArgs cArgs)
        confKey = cfoKey confOpts
        withLogger' = loggerBracket confKey lArgs . logException (lpDefaultName lArgs)
        -- FIXME
        -- There is a `Wlog.WithLogger` instance on `IO` defined in
        -- `Pos.Util.Wlog.Compatibility`. We can get a `Trace` from it, but
        -- it will `error` if used before logging is initialized by
        -- `withLogger'`. I cannot complain enough about this. Somebody chose
        -- global uninitialized mutable state, just because they wanted to be
        -- able to magically log in any `IO`. But the use of `error`s in that
        -- instance makes it obvious that it's all a lie: you can't log anytime
        -- anywhere, so why claim that you can? Just to avoid passing a
        -- parameter? To "write less code"?
        logTrace :: Trace IO (Severity, Text)
        logTrace = fromTypeclassWlog
        traceInfo :: Trace IO Text
        traceInfo = contramap ((,) Info) logTrace
        withConfigurations' = withConfigurations
            traceInfo
            (AssetLockPath <$> cnaAssetLockPath cArgs)
            (cnaDumpGenesisDataPath cArgs)
            (cnaDumpConfiguration cArgs)
            confOpts

    withLogger' $ withConfigurations' $ \genesisConfig walletConfig txpConfig ntpConfig -> do
        (nodeParams, Just sscParams) <- getNodeParams
            logTrace
            (lpDefaultName lArgs)
            cArgs
            nArgs
            (configGeneratedSecrets genesisConfig)

        let action' = action
                genesisConfig
                walletConfig
                txpConfig
                ntpConfig
                nodeParams
                sscParams

        bracketNodeResources
            genesisConfig
            nodeParams
            sscParams
            (txpGlobalSettings genesisConfig txpConfig)
            (initNodeDBs genesisConfig)
            action'


-- | Run basic core node
actionWithCoreNode
    :: (HasConfigurations, HasCompileInfo)
    => (Diffusion IO -> IO a)
    -> Genesis.Config
    -> WalletConfiguration
    -> TxpConfiguration
    -> NtpConfiguration
    -> NodeParams
    -> SscParams
    -> NodeResources EmptyMempoolExt
    -> IO ()
actionWithCoreNode action genesisConfig _ txpConfig _ _ _ nodeRes = do
    let plugins = [ ("update trigger", updateTriggerWorker) ]

    logInfo "Wallet is disabled, because software is built w/o it"
    let pm = configProtocolMagic genesisConfig
    runRealMode updateConfiguration genesisConfig txpConfig nodeRes $ \diff -> do
        let diff' = hoistDiffusion (elimRealMode updateConfiguration pm nodeRes diff') liftIO diff
        concurrently_
            (runNode genesisConfig txpConfig nodeRes plugins diff)
            (liftIO (action diff'))
