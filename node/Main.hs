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
import           Formatting (sformat, shown, (%))
import           Mockable (Production, currentTime, runProduction)
import           System.Wlog (LoggerName, logInfo)

import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), SimpleNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Core (GenesisData (..), Timestamp (..), genesisData)
import           Pos.Launcher (HasConfigurations, NodeParams (..), loggerBracket, runNodeReal,
                               withConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Update (updateTriggerWorker)
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
    runNodeReal nodeParams sscParams updateTriggerWorker

action
    :: ( HasConfigurations
       , HasCompileInfo
       )
    => SimpleNodeArgs
    -> Production ()
action (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) = do
    whenJust cnaDumpGenesisDataPath $ CLI.dumpGenesisData True
    logInfo $ sformat ("System start time is " % shown) $ gdStartTime genesisData
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- CLI.getNodeParams loggerName cArgs nArgs
    logInfo "Wallet is disabled, because software is built w/o it"
    logInfo $ sformat ("Using configs and genesis:\n"%shown) (CLI.configurationOptions (CLI.commonArgs cArgs))

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

    actionWithoutWallet sscParams currentParams

main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    let loggingParams = CLI.loggingParams loggerName commonNodeArgs
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    loggerBracket loggingParams . runProduction $ do
        CLI.printFlags
        withConfigurations conf $ action args
