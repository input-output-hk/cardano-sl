{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Universum

import           Data.Maybe          (fromJust)
import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import           Pos.Binary          ()
import qualified Pos.Client.CLI      as CLI
import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Constants       (isDevelopment)
import           Pos.Core            (gdStartTime, genesisData)
import           Pos.Explorer        (runExplorerBListener)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Web    (ExplorerProd, explorerPlugin, notifierPlugin)
import           Pos.Launcher        (HasConfigurations, NodeParams (..),
                                      NodeResources (..), bracketNodeResources,
                                      hoistNodeResources, loggerBracket, runNode,
                                      runRealBasedMode, withConfigurations)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Types           (Timestamp (Timestamp))
import           Pos.Update          (updateTriggerWorker)
import           Pos.Util            (inAssertMode, logException, mconcatPair)
import           Pos.Util.UserSecret (usVss)

import           ExplorerOptions     (Args (..), ExplorerNodeArgs (..),
                                      getExplorerOptions)
import           Params              (getNodeParams, gtSscParams)

printFlags :: IO ()
printFlags = do
    if isDevelopment
        then putText "[Attention] We are in DEV mode"
        else putText "[Attention] We are in PRODUCTION mode"
    inAssertMode $ putText "Asserts are ON"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getExplorerOptions
    let loggingParams = CLI.loggingParams "node" (enaCommonNodeArgs args)
    loggerBracket loggingParams . logException "node" $ do
        printFlags
        runProduction $ action (enaExplorerArgs args)

action :: Args -> Production ()
action args@Args {..} = withConfigurations conf $ do
    logInfo $ sformat ("System start time is " % shown) $ gdStartTime genesisData
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    nodeParams <- getNodeParams args
    putText $ "Static peers is on: " <> show staticPeers
    logInfo $ sformat ("Using configs and genesis:\n"%shown) conf

    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let sscParams = gtSscParams args vssSK

    let plugins :: HasConfigurations => ([WorkerSpec ExplorerProd], OutSpecs)
        plugins = mconcatPair
            [ explorerPlugin webPort
            , notifierPlugin NotifierSettings{ nsPort = notifierPort }
            , updateTriggerWorker
            ]

    bracketNodeResources nodeParams sscParams $ \nr@NodeResources {..} ->
        runExplorerRealMode
            (hoistNodeResources (lift . runExplorerBListener) nr)
            (runNode @SscGodTossing nr plugins)
  where
    conf = CLI.configurationOptions commonArgs
    runExplorerRealMode
        :: HasConfigurations
        => NodeResources SscGodTossing ExplorerProd
        -> (WorkerSpec ExplorerProd, OutSpecs)
        -> Production ()
    runExplorerRealMode = runRealBasedMode runExplorerBListener lift
