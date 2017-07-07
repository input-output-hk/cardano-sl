{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Main where

import           Universum

import           Control.Lens        (views)
import           Data.Maybe          (fromJust)
import           Ether.Internal      (HasLens (..))
import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import           Pos.Binary          ()
import qualified Pos.CLI             as CLI
import           Pos.Communication   (OutSpecs, WorkerSpec, worker)
#ifndef DEV_MODE
import           Pos.Genesis         (genesisStakeDistribution)
#endif
import           Pos.Launcher        (NodeParams (..), runNodeReal)
import           Pos.Shutdown        (triggerShutdown)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Types           (Timestamp (Timestamp))
import           Pos.Update.Context  (UpdateContext, ucUpdateSemaphore)
import           Pos.Util            (inAssertMode)
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (RealMode)

import           ExplorerOptions     (Args (..), getExplorerOptions)
import           Params              (getNodeParams, gtSscParams)

updateTriggerWorker :: ([WorkerSpec (RealMode SscGodTossing)], OutSpecs)
updateTriggerWorker = first pure $ worker mempty $ \_ -> do
    logInfo "Update trigger worker is locked"
    void $ takeMVar =<< views (lensOf @UpdateContext) ucUpdateSemaphore
    triggerShutdown

printFlags :: IO ()
printFlags = do
#ifdef DEV_MODE
    putText "[Attention] We are in DEV mode"
#else
    putText "[Attention] We are in PRODUCTION mode"
#endif
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
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText $ "Static peers is on: " <> show staticPeers

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK

    let plugins = updateTriggerWorker
    runNodeReal @SscGodTossing currentParams gtParams plugins
