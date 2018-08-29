{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)

import           Ntp.Client (NtpConfiguration)

import           Pos.Binary ()
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Core as Core (Config (..))
import           Pos.Launcher (HasConfigurations, NodeParams (..),
                     loggerBracket, runNodeRealSimple, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Util.Wlog (LoggerName, logInfo)
import           Pos.Worker.Update (updateTriggerWorker)

loggerName :: LoggerName
loggerName = "node"

actionWithoutWallet
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => Core.Config
    -> TxpConfiguration
    -> SscParams
    -> NodeParams
    -> IO ()
actionWithoutWallet coreConfig txpConfig sscParams nodeParams =
    runNodeRealSimple coreConfig txpConfig nodeParams sscParams [updateTriggerWorker]

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SimpleNodeArgs
    -> Core.Config
    -> TxpConfiguration
    -> NtpConfiguration
    -> IO ()
action (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) coreConfig txpConfig ntpConfig = do
    CLI.printInfoOnStart cArgs ntpConfig txpConfig
    logInfo "Wallet is disabled, because software is built w/o it"
    currentParams <- CLI.getNodeParams loggerName
                                       cArgs
                                       nArgs
                                       (configGeneratedSecrets coreConfig)

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

    actionWithoutWallet coreConfig txpConfig sscParams currentParams

main :: IO ()
main = withCompileInfo $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams loggerName commonNodeArgs
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    let blPath = AssetLockPath <$> cnaAssetLockPath commonNodeArgs
    loggerBracket loggingParams . logException "node" $
        withConfigurations blPath conf $ action args
