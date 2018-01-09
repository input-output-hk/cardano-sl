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
import           Formatting (sformat, shown, (%))
import           Mockable (Production, currentTime, runProduction)
import           System.Wlog (LoggerName, logInfo)

import           NodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..), getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (OutSpecs, WorkerSpec)
import           Pos.Core (Timestamp (Timestamp), gdStartTime, genesisData)
import           Pos.Explorer.DB (explorerInitDB)
import           Pos.Explorer.ExtraContext (makeExtraCtx)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Txp (ExplorerExtra, explorerTxpGlobalSettings)
import           Pos.Explorer.Web (ExplorerProd, explorerPlugin, liftToExplorerProd, notifierPlugin,
                                   runExplorerProd)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, hoistNodeResources,
                               loggerBracket, runNode, runRealBasedMode, withConfigurations)
import           Pos.Update (updateTriggerWorker)
import           Pos.Util (logException, mconcatPair)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)

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
        CLI.printFlags
        logInfo "[Attention] Software is built with explorer part"
        action args

action :: ExplorerNodeArgs -> Production ()
action (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) =
    withConfigurations conf $
    withCompileInfo $(retrieveCompileTimeInfo) $ do
        let systemStart = gdStartTime genesisData
        logInfo $ sformat ("System start time is " % shown) systemStart
        t <- currentTime
        logInfo $ sformat ("Current time is " % shown) (Timestamp t)
        currentParams <- getNodeParams loggerName cArgs nodeArgs
        logInfo $ "Explorer is enabled!"
        logInfo $ sformat ("Using configs and genesis:\n"%shown) conf

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
            let extraCtx = makeExtraCtx
            in runExplorerRealMode
                (hoistNodeResources (liftToExplorerProd . runExplorerProd extraCtx) nr)
                (runNode nr plugins)
  where

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runExplorerRealMode
        :: (HasConfigurations,HasCompileInfo)
        => NodeResources ExplorerExtra ExplorerProd
        -> (WorkerSpec ExplorerProd, OutSpecs)
        -> Production ()
    runExplorerRealMode nr@NodeResources{..} =
        let extraCtx = makeExtraCtx
        in runRealBasedMode (runExplorerProd extraCtx) liftToExplorerProd nr

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
