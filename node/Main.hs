{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)
import           Mockable (Production (..), runProduction)
import           System.Wlog (LoggerName, logInfo)

import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Launcher (HasConfigurations, NodeParams (..), loggerBracket, runNodeReal,
                               withConfigurations)
import           Pos.Ntp.Configuration (NtpConfiguration)
import           Pos.Ssc.Types (SscParams)
import           Pos.Update.Worker (updateTriggerWorker)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)

loggerName :: LoggerName
loggerName = "node"

actionWithoutWallet
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SscParams
    -> NodeParams
    -> Production ()
actionWithoutWallet sscParams nodeParams =
    Production $ runNodeReal nodeParams sscParams [updateTriggerWorker]

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SimpleNodeArgs
    -> NtpConfiguration
    -> Production ()
action (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) ntpConfig = do
    CLI.printInfoOnStart cArgs ntpConfig
    logInfo "Wallet is disabled, because software is built w/o it"
    currentParams <- CLI.getNodeParams loggerName cArgs nArgs

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

    actionWithoutWallet sscParams currentParams

main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams loggerName commonNodeArgs
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    loggerBracket loggingParams . logException "node" . runProduction $
        withConfigurations conf $ action args
