{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Universum

import           Data.Default        (def)
import           Data.Maybe          (fromJust)
import           Formatting          (build, sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import           Pos.Binary          ()
import qualified Pos.Client.CLI      as CLI
import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Constants       (isDevelopment)
import           Pos.Core            (HasCoreConstants, giveStaticConsts)
import           Pos.Explorer        (runExplorerBListener)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Web    (ExplorerProd, explorerPlugin, notifierPlugin)
import           Pos.Launcher        (NodeParams (..), NodeResources (..),
                                      applyConfigInfo, bracketNodeResources,
                                      hoistNodeResources, runNode, runRealBasedMode)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Types           (Timestamp (Timestamp))
import           Pos.Update          (updateTriggerWorker)
import           Pos.Util            (inAssertMode, mconcatPair)
import           Pos.Util.UserSecret (usVss)

import           ExplorerOptions     (Args (..), getExplorerOptions)
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
    printFlags
    runProduction (action args)

action :: Args -> Production ()
action args@Args {..} = do
    let configInfo = def
    liftIO $ applyConfigInfo configInfo
    giveStaticConsts $ do
        systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
        logInfo $ sformat ("System start time is " % shown) systemStart
        t <- currentTime
        logInfo $ sformat ("Current time is " % shown) (Timestamp t)
        nodeParams <- getNodeParams args systemStart
        putText $ "Static peers is on: " <> show staticPeers
        logInfo $ sformat ("Using configs and genesis:\n"%build) configInfo

        let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
        let sscParams = gtSscParams args vssSK

        let plugins :: HasCoreConstants => ([WorkerSpec ExplorerProd], OutSpecs)
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
    runExplorerRealMode
        :: HasCoreConstants
        => NodeResources SscGodTossing ExplorerProd
        -> (WorkerSpec ExplorerProd, OutSpecs)
        -> Production ()
    runExplorerRealMode = runRealBasedMode runExplorerBListener lift
