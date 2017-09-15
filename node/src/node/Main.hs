{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe          (fromJust)
import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)

import           Pos.Binary          ()
import           Pos.Client.CLI      (CommonNodeArgs (..), NodeArgs (..),
                                      SimpleNodeArgs (..))
import qualified Pos.Client.CLI      as CLI
import           Pos.Communication   (OutSpecs, WorkerSpec)
import           Pos.Core            (Timestamp (..), systemStart)
import           Pos.Launcher        (HasConfigurations, NodeParams (..), runNodeReal,
                                      withConfigurations)
import           Pos.Ssc.Class       (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.NistBeacon  (SscNistBeacon)
import           Pos.Ssc.SscAlgo     (SscAlgo (..))
import           Pos.Update          (updateTriggerWorker)
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (RealMode)

actionWithoutWallet
    :: forall ssc.
       ( SscConstraint ssc
       , HasConfigurations
       )
    => SscParams ssc
    -> NodeParams
    -> Production ()
actionWithoutWallet sscParams nodeParams =
    runNodeReal @ssc nodeParams sscParams plugins
  where
    plugins :: ([WorkerSpec (RealMode ssc)], OutSpecs)
    plugins = updateTriggerWorker

action
    :: ( HasConfigurations )
    => SimpleNodeArgs
    -> Production ()
action (SimpleNodeArgs (cArgs@CommonNodeArgs {..}) (nArgs@NodeArgs {..})) = do
    liftIO $ applyConfigInfo configInfo
    giveStaticConsts $ do
        systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
        whenJust cnaDumpGenesisDataPath $ CLI.dumpGenesisData systemStart
        putText $ sformat ("System start time is " % shown) systemStart
        t <- currentTime
        putText $ sformat ("Current time is " % shown) (Timestamp t)
        currentParams <- CLI.getNodeParams cArgs nArgs systemStart
        putText $ "Running using " <> show sscAlgo
        putText "Wallet is disabled, because software is built w/o it"
        putText $ sformat ("Using configs and genesis:\n"%build) configInfo

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

    case sscAlgo of
        NistBeaconAlgo ->
            actionWithoutWallet @SscNistBeacon () currentParams
        GodTossingAlgo ->
            actionWithoutWallet @SscGodTossing gtParams currentParams

main :: IO ()
main = do
    args@(CLI.SimpleNodeArgs commonNodeArgs _) <- CLI.getSimpleNodeOptions
    CLI.printFlags
    let conf = CLI.configurationOptions (CLI.commonArgs commonNodeArgs)
    runProduction $ withConfigurations conf (action args)
