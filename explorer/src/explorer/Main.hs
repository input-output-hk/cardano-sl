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
import           Mockable (Production (..), runProduction)
import           System.Wlog (LoggerName, logInfo)

import           ExplorerNodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..),
                                      getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Core (epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.Explorer.DB (explorerInitDB)
import           Pos.Explorer.ExtraContext (makeExtraCtx)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Txp (ExplorerExtraModifier, explorerTxpGlobalSettings)
import           Pos.Explorer.Web (ExplorerProd, explorerPlugin, notifierPlugin, runExplorerProd)
import           Pos.Infra.Diffusion.Types (Diffusion, hoistDiffusion)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, loggerBracket, runNode,
                               runRealMode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..))
import           Pos.Update.Worker (updateTriggerWorker)
import           Pos.Util (logException)
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
        logInfo "[Attention] Software is built with explorer part"
        action args

action :: ExplorerNodeArgs -> Production ()
action (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) =
    withConfigurations blPath conf $ \ntpConfig pm ->
    withCompileInfo $(retrieveCompileTimeInfo) $ do
        CLI.printInfoOnStart cArgs ntpConfig
        logInfo $ "Explorer is enabled!"
        currentParams <- getNodeParams loggerName cArgs nodeArgs

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        let plugins :: [Diffusion ExplorerProd -> ExplorerProd ()]
            plugins =
                [ explorerPlugin pm webPort
                , notifierPlugin NotifierSettings{ nsPort = notifierPort }
                , updateTriggerWorker
                ]
        bracketNodeResources currentParams sscParams
            (explorerTxpGlobalSettings pm)
            (explorerInitDB pm epochSlots) $ \nr@NodeResources {..} ->
                Production (runExplorerRealMode pm nr (runNode pm nr plugins))
  where

    blPath :: Maybe AssetLockPath
    blPath = AssetLockPath <$> cnaAssetLockPath

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs

    runExplorerRealMode
        :: (HasConfigurations,HasCompileInfo)
        => ProtocolMagic
        -> NodeResources ExplorerExtraModifier
        -> (Diffusion ExplorerProd -> ExplorerProd ())
        -> IO ()
    runExplorerRealMode pm nr@NodeResources{..} go =
        let NodeContext {..} = nrContext
            extraCtx = makeExtraCtx
            explorerModeToRealMode  = runExplorerProd extraCtx
         in runRealMode pm nr $ \diffusion ->
                explorerModeToRealMode (go (hoistDiffusion (lift . lift) explorerModeToRealMode diffusion))

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }
