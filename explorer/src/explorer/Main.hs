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

import           ExplorerNodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..),
                     getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI (CommonNodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Explorer.DB (explorerInitDB)
import           Pos.Explorer.ExtraContext (makeExtraCtx)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Txp (ExplorerExtraModifier,
                     explorerTxpGlobalSettings)
import           Pos.Explorer.Web (ExplorerProd, explorerPlugin, notifierPlugin,
                     runExplorerProd)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeResources (..), bracketNodeResources, loggerBracket,
                     runNode, runRealMode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Trace (fromTypeclassWlog, noTrace)
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
    loggerBracket "explorer" loggingParams . logException "node" $ do
        logInfo "[Attention] Software is built with explorer part"
        action args

action :: ExplorerNodeArgs -> IO ()
action (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) =
    withConfigurations noTrace blPath cnaDumpGenesisDataPath cnaDumpConfiguration conf $ \genesisConfig _ txpConfig _ntpConfig ->
    withCompileInfo $ do
        logInfo $ "Explorer is enabled!"
        (currentParams, Just sscParams) <- getNodeParams
            fromTypeclassWlog
            loggerName
            cArgs
            (CLI.NodeArgs { behaviorConfigPath = Nothing })
            (configGeneratedSecrets genesisConfig)

        let plugins :: [ (Text, Diffusion ExplorerProd -> ExplorerProd ()) ]
            plugins =
                [ ("explorer plugin", explorerPlugin genesisConfig webPort)
                , ("explorer notifier", notifierPlugin genesisConfig NotifierSettings {nsPort = notifierPort})
                , ("explorer update trigger", updateTriggerWorker)
                ]
        bracketNodeResources
            genesisConfig
            currentParams
            sscParams
            (explorerTxpGlobalSettings genesisConfig txpConfig)
            (explorerInitDB genesisConfig) $ \nr@NodeResources {..} ->
                runExplorerRealMode genesisConfig txpConfig nr (runNode genesisConfig txpConfig nr plugins)
  where

    blPath :: Maybe AssetLockPath
    blPath = AssetLockPath <$> cnaAssetLockPath

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runExplorerRealMode
        :: (HasConfigurations,HasCompileInfo)
        => Genesis.Config
        -> TxpConfiguration
        -> NodeResources ExplorerExtraModifier
        -> (Diffusion ExplorerProd -> ExplorerProd ())
        -> IO ()
    runExplorerRealMode genesisConfig txpConfig nr@NodeResources{..} go =
        let NodeContext {..} = nrContext
            extraCtx = makeExtraCtx genesisConfig
            explorerModeToRealMode  = runExplorerProd extraCtx
         in runRealMode updateConfiguration genesisConfig txpConfig nr $ \diffusion ->
                explorerModeToRealMode (go (hoistDiffusion (lift . lift) explorerModeToRealMode diffusion))
