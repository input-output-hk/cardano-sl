{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)
import           Mockable (Production, runProduction)
import           System.Wlog (LoggerName, logInfo)

import           ExplorerNodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..),
                                      getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (OutSpecs)
import           Pos.Context (NodeContext (..))
import           Pos.Explorer.DB (explorerInitDB)
import           Pos.Explorer.ExtraContext (makeExtraCtx)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Txp (ExplorerExtraModifier, explorerTxpGlobalSettings)
import           Pos.Explorer.Web (ExplorerProd, explorerPlugin, notifierPlugin, runExplorerProd)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, elimRealMode,
                               loggerBracket, runNode, runServer, withConfigurations)
import           Pos.Reporting.Ekg (EkgNodeMetrics (..))
import           Pos.Update.Worker (updateTriggerWorker)
import           Pos.Util (logException, mconcatPair)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Worker.Types (WorkerSpec)

loggerName :: LoggerName
loggerName = "node"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getExplorerNodeOptions
    let loggingParams = CLI.loggingParams loggerName (enaCommonNodeArgs args)
    loggerBracket loggingParams . logException "node" . runProduction $ do
        logInfo "[Attention] Software is built with explorer part"
        action args

action :: ExplorerNodeArgs -> Production ()
action (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) =
    withConfigurations conf $ \ntpConfig ->
    withCompileInfo $(retrieveCompileTimeInfo) $ do
        CLI.printInfoOnStart cArgs ntpConfig
        logInfo $ "Explorer is enabled!"
        currentParams <- getNodeParams loggerName cArgs nodeArgs

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        let plugins :: HasConfigurations => ([WorkerSpec ExplorerProd], OutSpecs)
            plugins = mconcatPair
                [ explorerPlugin webPort
                , notifierPlugin NotifierSettings{ nsPort = notifierPort }
                , updateTriggerWorker
                ]
        bracketNodeResources currentParams sscParams
            explorerTxpGlobalSettings
            explorerInitDB $ \nr@NodeResources {..} ->
                runExplorerRealMode nr (runNode nr plugins)
  where

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runExplorerRealMode
        :: (HasConfigurations,HasCompileInfo)
        => NodeResources ExplorerExtraModifier
        -> (WorkerSpec ExplorerProd, OutSpecs)
        -> Production ()
    runExplorerRealMode nr@NodeResources{..} (go, outSpecs) =
        let NodeContext {..} = nrContext
            extraCtx = makeExtraCtx
            explorerModeToRealMode  = runExplorerProd extraCtx
            elim = elimRealMode nr
            ekgNodeMetrics = EkgNodeMetrics
                nrEkgStore
            serverRealMode = explorerModeToRealMode $ runServer
                (runProduction . elim . explorerModeToRealMode)
                ncNodeParams
                ekgNodeMetrics
                outSpecs
                go
        in  elim serverRealMode

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
