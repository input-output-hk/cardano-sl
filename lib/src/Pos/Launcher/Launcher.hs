{-# LANGUAGE RankNTypes      #-}
{-# LANGUAGE RecordWildCards #-}

-- | Applications of runners to scenarios.

module Pos.Launcher.Launcher
       ( -- * Node launcher.
       launchNode
       ) where

import           Universum

import           Ntp.Client (NtpConfiguration)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (configurationOptions)
import           Pos.Client.CLI.Params (getNodeParams)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp.Logic (txpGlobalSettings)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     HasConfigurations, WalletConfiguration, cfoKey,
                     withConfigurations)
import           Pos.Launcher.Param (LoggingParams (..), NodeParams (..))
import           Pos.Launcher.Resource (NodeResources, bracketNodeResources,
                     loggerBracket)
import           Pos.Util.Util (logException)
import           Pos.WorkMode (EmptyMempoolExt)

-- import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)


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
    let confKey = cfoKey confOpts
    let withLogger' = loggerBracket confKey lArgs . logException (lpDefaultName lArgs)
    let withConfigurations' = withConfigurations
            (AssetLockPath <$> cnaAssetLockPath cArgs)
            (cnaDumpGenesisDataPath cArgs)
            (cnaDumpConfiguration cArgs)
            confOpts

    withLogger' $ withConfigurations' $ \genesisConfig walletConfig txpConfig ntpConfig -> do
        (nodeParams, Just sscParams) <- getNodeParams
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
