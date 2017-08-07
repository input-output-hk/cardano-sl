{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum           hiding (over)

import           Data.Maybe          (fromJust)
import           Formatting          (sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logError, logInfo)

import           Pos.Binary          ()
import qualified Pos.CLI             as CLI
import           Pos.Communication   (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Context         (HasNodeContext)
import           Pos.Core.Types      (Timestamp (..))
import           Pos.Launcher        (NodeParams (..), NodeResources (..),
                                      bracketNodeResources, runNode)
import           Pos.Ssc.Class       (SscParams)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Ssc.NistBeacon  (SscNistBeacon)
import           Pos.Ssc.SscAlgo     (SscAlgo (..))
import           Pos.Util.UserSecret (usVss)
import           Pos.WorkMode        (WorkMode)
import           Pos.Web             (serveWebGT)
import           Pos.Wallet.Web      (WalletWebMode, bracketWalletWS, bracketWalletWebDB,
                                      runWRealMode, walletServeWebFull, walletServerOuts)

import           Pos.Client.CLI.NodeOptions (Args (..), getNodeOptions)
import           Pos.Client.CLI.Params (getNodeParams, gtSscParams)
import           Pos.Client.CLI.Util (printFlags)

----------------------------------------------------------------------------
-- With wallet
----------------------------------------------------------------------------

actionWithWallet :: SscParams SscGodTossing -> NodeParams -> Args -> Production ()
actionWithWallet sscParams nodeParams args@Args {..} =
    bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
        bracketWalletWS $ \conn -> do
            bracketNodeResources nodeParams sscParams $ \nr@NodeResources {..} ->
                runWRealMode
                    db
                    conn
                    nr
                    (runNode @SscGodTossing nr plugins)
  where
    convPlugins = (, mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)
    plugins :: ([WorkerSpec WalletWebMode], OutSpecs)
    plugins = convPlugins (pluginsGT args) <> walletProd args

walletProd :: Args -> ([WorkerSpec WalletWebMode], OutSpecs)
walletProd Args {..} = first pure $ worker walletServerOuts $ \sendActions ->
    walletServeWebFull
        sendActions
        walletDebug
        walletPort
        walletTLSCertPath
        walletTLSKeyPath
        walletTLSCAPath

pluginsGT ::
    ( WorkMode SscGodTossing ctx m
    , HasNodeContext SscGodTossing ctx
    ) => Args -> [m ()]
pluginsGT Args {..}
    | enableWeb = [serveWebGT webPort walletTLSCertPath walletTLSKeyPath walletTLSCAPath]
    | otherwise = []

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

main :: IO ()
main = do
    args <- getNodeOptions
    printFlags
    putText "[Attention] Software is built with wallet part"
    runProduction (action args)

action :: Args -> Production ()
action args@Args {..} = do
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getNodeParams args systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText $ "Is wallet enabled: true "

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams args vssSK

    let sscParams :: Either (SscParams SscNistBeacon) (SscParams SscGodTossing)
        sscParams = bool (Left ()) (Right gtParams) (CLI.sscAlgo commonArgs == GodTossingAlgo)
    case sscParams of
        (Left _)     -> logError "Wallet does not support NIST beacon!"
        (Right par)  -> actionWithWallet par currentParams args
