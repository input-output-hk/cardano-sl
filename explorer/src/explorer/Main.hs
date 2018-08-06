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
import           System.Wlog (LoggerName, logInfo)

import           ExplorerNodeOptions (ExplorerArgs (..), ExplorerNodeArgs (..),
                     getExplorerNodeOptions)
import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..),
                     getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (NodeContext (..))
import           Pos.Core (epochSlots)
import           Pos.Crypto (ProtocolMagic)
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
    withConfigurations blPath conf $ \ntpConfig pm ->
    withCompileInfo $ do
        CLI.printInfoOnStart cArgs ntpConfig
        logInfo "Explorer is enabled!"
        (currentParams, Just sscParams) <- getNodeParams loggerName cArgs nodeArgs

        let plugins :: [Diffusion ExplorerProd -> ExplorerProd ()]
            plugins =
                [ explorerPlugin webPort
                , notifierPlugin NotifierSettings{ nsPort = notifierPort }
                , updateTriggerWorker
                ]
        bracketNodeResources currentParams sscParams
            (explorerTxpGlobalSettings pm)
            (explorerInitDB pm epochSlots) $ \nr@NodeResources {..} ->
                runExplorerRealMode pm nr (runNode pm nr plugins)
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
