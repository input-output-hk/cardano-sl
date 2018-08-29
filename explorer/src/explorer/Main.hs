{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)

import           ExplorerNodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..),
                     getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Core as Core (Config (..))
import           Pos.Explorer.DB (explorerInitDB)
import           Pos.Explorer.ExtraContext (makeExtraCtx)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Txp (ExplorerExtraModifier,
                     explorerTxpGlobalSettings)
import           Pos.Explorer.Web (ExplorerProd, explorerPlugin, notifierPlugin,
                     runExplorerProd)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), NodeResources (..), bracketNodeResources,
                     loggerBracket, runNode, runRealMode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Util.Wlog (LoggerName, logInfo)
import           Pos.Worker.Update (updateTriggerWorker)

loggerName :: LoggerName
loggerName = "node"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getExplorerNodeOptions
    let loggingParams = CLI.loggingParams loggerName (enaCommonNodeArgs args)
    loggerBracket loggingParams . logException "node" $ do
        logInfo "[Attention] Software is built with explorer part"
        action args

action :: ExplorerNodeArgs -> IO ()
action (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) =
    withConfigurations blPath conf $ \coreConfig txpConfig ntpConfig ->
    withCompileInfo $ do
        CLI.printInfoOnStart cArgs
                             (configGenesisData coreConfig)
                             ntpConfig
                             txpConfig
        logInfo $ "Explorer is enabled!"
        currentParams <- getNodeParams
            loggerName
            cArgs
            nodeArgs
            (configGeneratedSecrets coreConfig)

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        let plugins :: [Diffusion ExplorerProd -> ExplorerProd ()]
            plugins =
                [ explorerPlugin coreConfig webPort
                , notifierPlugin coreConfig NotifierSettings {nsPort = notifierPort}
                , updateTriggerWorker
                ]
        bracketNodeResources
            coreConfig
            currentParams
            sscParams
            (explorerTxpGlobalSettings coreConfig txpConfig)
            (explorerInitDB coreConfig) $ \nr@NodeResources {..} ->
                runExplorerRealMode coreConfig txpConfig nr (runNode coreConfig txpConfig nr plugins)
  where

    blPath :: Maybe AssetLockPath
    blPath = AssetLockPath <$> cnaAssetLockPath

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runExplorerRealMode
        :: (HasConfigurations,HasCompileInfo)
        => Core.Config
        -> TxpConfiguration
        -> NodeResources ExplorerExtraModifier
        -> (Diffusion ExplorerProd -> ExplorerProd ())
        -> IO ()
    runExplorerRealMode coreConfig txpConfig nr@NodeResources{..} go =
        let NodeContext {..} = nrContext
            extraCtx = makeExtraCtx coreConfig
            explorerModeToRealMode  = runExplorerProd extraCtx
         in runRealMode coreConfig txpConfig nr $ \diffusion ->
                explorerModeToRealMode (go (hoistDiffusion (lift . lift) explorerModeToRealMode diffusion))

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
