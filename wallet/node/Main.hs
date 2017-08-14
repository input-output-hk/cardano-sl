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

import           Data.Maybe                 (fromJust)
import           Formatting                 (sformat, shown, (%))
import           Mockable                   (Production, currentTime, runProduction)
import           System.Wlog                (logError, logInfo)

import           Pos.Binary                 ()
import qualified Pos.CLI                    as CLI
import           Pos.Communication          (ActionSpec (..), OutSpecs, WorkerSpec,
                                             worker)
import           Pos.Context                (HasNodeContext)
import           Pos.Core                   (HasCoreConstants, Timestamp (..),
                                             giveStaticConsts)
import           Pos.Launcher               (NodeParams (..), NodeResources (..),
                                             bracketNodeResources, runNode)
import           Pos.Ssc.Class              (SscParams)
import           Pos.Ssc.GodTossing         (SscGodTossing)
import           Pos.Ssc.NistBeacon         (SscNistBeacon)
import           Pos.Ssc.SscAlgo            (SscAlgo (..))
import           Pos.Util.UserSecret        (usVss)
import           Pos.Wallet.Web             (WalletWebMode, bracketWalletWS,
                                             bracketWalletWebDB, runWRealMode,
                                             walletServeWebFull, walletServerOuts)
import           Pos.Web                    (serveWebGT)
import           Pos.WorkMode               (WorkMode)

import           Pos.Client.CLI.NodeOptions (SimpleNodeArgs (..))
import           Pos.Client.CLI.Params      (getSimpleNodeParams, gtSscParams)
import           Pos.Client.CLI.Util        (printFlags)

import           NodeOptions                (WalletArgs (..), WalletNodeArgs (..),
                                             getWalletNodeOptions)

actionWithWallet :: SscParams SscGodTossing -> NodeParams -> WalletArgs -> Production ()
actionWithWallet sscParams nodeParams wArgs@WalletArgs {..} =
    bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
        bracketWalletWS $ \conn -> giveStaticConsts $
            bracketNodeResources nodeParams sscParams $ \nr@NodeResources {..} ->
                runWRealMode
                    db
                    conn
                    nr
                    (runNode @SscGodTossing nr plugins)
  where
    convPlugins = (, mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)
    plugins :: HasCoreConstants => ([WorkerSpec WalletWebMode], OutSpecs)
    plugins = convPlugins (pluginsGT wArgs) <> walletProd wArgs

walletProd :: HasCoreConstants => WalletArgs -> ([WorkerSpec WalletWebMode], OutSpecs)
walletProd WalletArgs {..} = first pure $ worker walletServerOuts $ \sendActions ->
    walletServeWebFull
        sendActions
        walletDebug
        walletPort
        (Just walletTLSParams)

pluginsGT ::
    ( WorkMode SscGodTossing ctx m
    , HasNodeContext SscGodTossing ctx
    ) => WalletArgs -> [m ()]
pluginsGT WalletArgs {..}
    | enableWeb = [serveWebGT webPort (Just walletTLSParams)]
    | otherwise = []

action :: WalletNodeArgs -> Production ()
action (WalletNodeArgs (snArgs@SimpleNodeArgs {..}) (wArgs@WalletArgs {..})) = do
    systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
    logInfo $ sformat ("System start time is " % shown) systemStart
    t <- currentTime
    logInfo $ sformat ("Current time is " % shown) (Timestamp t)
    currentParams <- getSimpleNodeParams snArgs systemStart
    putText $ "Running using " <> show (CLI.sscAlgo commonArgs)
    putText $ "Wallet is enabled!"

    let vssSK = fromJust $ npUserSecret currentParams ^. usVss
    let gtParams = gtSscParams snArgs vssSK

    let sscParams :: Either (SscParams SscNistBeacon) (SscParams SscGodTossing)
        sscParams = bool (Left ()) (Right gtParams) (CLI.sscAlgo commonArgs == GodTossingAlgo)
    case sscParams of
        (Left _)    -> logError "Wallet does not support NIST beacon!"
        (Right par) -> actionWithWallet par currentParams wArgs

main :: IO ()
main = do
    args <- getWalletNodeOptions
    printFlags
    putText "[Attention] Software is built with wallet part"
    runProduction (action args)
