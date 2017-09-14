{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe          (fromJust)
import           Ether.Internal      (HasLens (..))
import           Formatting          (build, sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import           NodeOptions         (ExplorerArgs (..), ExplorerNodeArgs (..),
                                      getExplorerNodeOptions)
import           Pos.Binary          ()
import           Pos.Client.CLI      (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI      as CLI
import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Core            (HasCoreConstants, giveStaticConsts)
import           Pos.Explorer        (runExplorerBListener)
import           Pos.Explorer.Socket (NotifierSettings (..))
import           Pos.Explorer.Web    (ExplorerProd, explorerPlugin, notifierPlugin)
import           Pos.Launcher        (NodeParams (..), NodeResources (..),
                                      applyConfigInfo, bracketNodeResources,
                                      hoistNodeResources, runNode, runRealBasedMode)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.SscAlgo     (SscAlgo (..))
import           Pos.Types           (Timestamp (Timestamp))
import           Pos.Update          (updateTriggerWorker)
import           Pos.Util            (mconcatPair)
import           Pos.Util.UserSecret (usVss)


----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getExplorerNodeOptions
    CLI.printFlags
    putText "[Attention] Software is built with explorer part"
    runProduction (action args)

action :: ExplorerNodeArgs -> Production ()
action (ExplorerNodeArgs (cArgs@CommonNodeArgs{..}) ExplorerArgs{..}) = do
    liftIO $ applyConfigInfo configInfo
    giveStaticConsts $ do
        systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
        logInfo $ sformat ("System start time is " % shown) systemStart
        t <- currentTime
        logInfo $ sformat ("Current time is " % shown) (Timestamp t)
        currentParams <- getNodeParams cArgs nodeArgs systemStart
        putText $ "Explorer is enabled!"
        logInfo $ sformat ("Using configs and genesis:\n"%build) configInfo

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        let plugins :: HasCoreConstants => ([WorkerSpec ExplorerProd], OutSpecs)
            plugins = mconcatPair
                [ explorerPlugin webPort
                , notifierPlugin NotifierSettings{ nsPort = notifierPort }
                , updateTriggerWorker
                ]

        bracketNodeResources currentParams gtParams $ \nr@NodeResources {..} ->
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

    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { sscAlgo = GodTossingAlgo, behaviorConfigPath = Nothing }