{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum                  hiding (over)

import           Control.Lens               (views)
import           Data.Maybe                 (fromJust)
import           Ether.Internal             (HasLens (..))
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, currentTime, runProduction)
import           System.Wlog                (logInfo)

import           Pos.Binary                 ()
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (OutSpecs, WorkerSpec, worker)
import           Pos.Core                   (HasCoreConstants, Timestamp (..),
                                             giveStaticConsts)
import           Pos.Launcher               (NodeParams (..), runNodeReal)
import           Pos.Security               (SecurityWorkersClass)
import           Pos.Shutdown               (triggerShutdown)
import           Pos.Ssc.Class              (SscConstraint, SscParams)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.NistBeacon         (SscNistBeacon)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))
import           Pos.Update.Context         (UpdateContext, ucUpdateSemaphore)
import           Pos.Util.UserSecret        (usVss)
import           Pos.WorkMode               (RealMode)

import           Pos.Client.CLI.NodeOptions (SimpleNodeArgs (..), getSimpleNodeOptions)
import           Pos.Client.CLI.Params      (getSimpleNodeParams, gtSscParams)
import           Pos.Client.CLI.Util        (printFlags)

actionWithoutWallet
    :: forall ssc.
       ( SscConstraint ssc
       , SecurityWorkersClass ssc
       , HasCoreConstants)
    => SscParams ssc
    -> NodeParams
    -> Production ()
actionWithoutWallet sscParams nodeParams =
    runNodeReal @ssc nodeParams sscParams plugins
  where
    plugins :: ([WorkerSpec (RealMode ssc)], OutSpecs)
    plugins = updateTriggerWorker

updateTriggerWorker
    :: ([WorkerSpec (RealMode ssc)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< views (lensOf @UpdateContext) ucUpdateSemaphore
    triggerShutdown

action :: SimpleNodeArgs -> Production ()
action args@SimpleNodeArgs {..} = giveStaticConsts $ do
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getSimpleNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText "Wallet is disabled, because software is built w/o it"

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK

    let sscParams :: Either (SscParams SscNistBeacon) (SscParams SscGodTossing)
        sscParams = bool (Left ()) (Right gtParams) (CLI.sscAlgo commonArgs == GodTossingAlgo)
    case sscParams of
        Left par  -> actionWithoutWallet @SscNistBeacon par currentParams
        Right par -> actionWithoutWallet @SscGodTossing par currentParams

main :: IO ()
main = do
    args <- getSimpleNodeOptions
    printFlags
    runProduction (action args)
