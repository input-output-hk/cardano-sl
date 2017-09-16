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
import           Formatting          (build, sformat, shown, (%))
import           Mockable            (Production, currentTime, runProduction)
import           System.Wlog         (logInfo)

import           Pos.Binary          ()
import           Pos.Client.CLI      (CommonNodeArgs (..))
import qualified Pos.Client.CLI      as CLI
import           Pos.Communication   (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Context         (HasNodeContext)
import           Pos.Core            (HasCoreConstants, Timestamp (..), giveStaticConsts)
import           Pos.Launcher        (NodeParams (..), NodeResources (..),
                                      applyConfigInfo, bracketNodeResources, runNode)
import           Pos.Ssc.Class       (SscParams)
import           Pos.Ssc.GodTossing  (SscGodTossing)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web      (WalletWebMode, bracketWalletWS, bracketWalletWebDB,
                                      runWRealMode, walletServeWebFull, walletServerOuts)
import           Pos.Web             (serveWebGT)
import           Pos.WorkMode        (WorkMode)

import           NodeOptions         (WalletArgs (..), WalletNodeArgs (..),
                                      getWalletNodeOptions)
import           Params              (getNodeParams)

actionWithWallet :: HasCoreConstants => SscParams SscGodTossing -> NodeParams -> WalletArgs -> Production ()
actionWithWallet sscParams nodeParams wArgs@WalletArgs {..} =
    bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
        bracketWalletWS $ \conn ->
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
action (WalletNodeArgs (cArgs@CommonNodeArgs{..}) (wArgs@WalletArgs{..})) = do
    liftIO $ applyConfigInfo configInfo
    giveStaticConsts $ do
        systemStart <- CLI.getNodeSystemStart $ CLI.sysStart commonArgs
        logInfo $ sformat ("System start time is " % shown) systemStart
        t <- currentTime
        logInfo $ sformat ("Current time is " % shown) (Timestamp t)
        currentParams <- getNodeParams cArgs systemStart
        putText $ "Wallet is enabled!"
        logInfo $ sformat ("Using configs and genesis:\n"%build) configInfo

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        actionWithWallet gtParams currentParams wArgs

main :: IO ()
main = do
    args <- getWalletNodeOptions
    CLI.printFlags
    putText "[Attention] Software is built with wallet part"
    runProduction (action args)
