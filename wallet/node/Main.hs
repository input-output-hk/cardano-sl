{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum            hiding (over)

import           Data.Maybe           (fromJust)
import           Formatting           (sformat, shown, (%))
import           Mockable             (Production, currentTime, runProduction)
import           System.Wlog          (modifyLoggerName)

import           Pos.Binary           ()
import           Pos.Client.CLI       (CommonNodeArgs (..))
import qualified Pos.Client.CLI       as CLI
import           Pos.Communication    (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Context          (HasNodeContext)
import           Pos.Core             (Timestamp (..), gdStartTime, genesisData)
import           Pos.Launcher         (HasConfigurations, NodeParams (..),
                                       NodeResources (..), bracketNodeResources, runNode,
                                       withConfigurations)
import           Pos.Ssc.Class        (SscParams)
import           Pos.Ssc.GodTossing   (SscGodTossing)
import           Pos.Util.UserSecret  (usVss)
import           Pos.Wallet.Web       (WalletWebMode, bracketWalletWS, bracketWalletWebDB,
                                       runWRealMode, walletServeWebFull, walletServerOuts)
import           Pos.Wallet.Web.State (cleanupAcidStatePeriodically)
import           Pos.Web              (serveWebGT)
import           Pos.WorkMode         (WorkMode)

import           NodeOptions          (WalletArgs (..), WalletNodeArgs (..),
                                       getWalletNodeOptions)
import           Params               (getNodeParams)

actionWithWallet :: HasConfigurations => SscParams SscGodTossing -> NodeParams -> WalletArgs -> Production ()
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
    plugins :: HasConfigurations => ([WorkerSpec WalletWebMode], OutSpecs)
    plugins = mconcat [ convPlugins (pluginsGT wArgs)
                      , walletProd wArgs
                      , acidCleanupWorker wArgs ]

acidCleanupWorker :: HasConfigurations => WalletArgs -> ([WorkerSpec WalletWebMode], OutSpecs)
acidCleanupWorker WalletArgs{..} =
    first one $ worker mempty $ const $
    modifyLoggerName (const "acidcleanup") $
    cleanupAcidStatePeriodically walletAcidInterval

walletProd :: HasConfigurations => WalletArgs -> ([WorkerSpec WalletWebMode], OutSpecs)
walletProd WalletArgs {..} = first one $ worker walletServerOuts $ \sendActions ->
    walletServeWebFull
        sendActions
        walletDebug
        walletAddress
        (Just walletTLSParams)

pluginsGT ::
    ( WorkMode SscGodTossing ctx m
    , HasNodeContext SscGodTossing ctx
    , HasConfigurations
    ) => WalletArgs -> [m ()]
pluginsGT WalletArgs {..}
    | enableWeb = [serveWebGT webPort (Just walletTLSParams)]
    | otherwise = []

action :: WalletNodeArgs -> Production ()
action (WalletNodeArgs (cArgs@CommonNodeArgs{..}) (wArgs@WalletArgs{..})) =
    withConfigurations conf $ do
        whenJust cnaDumpGenesisDataPath $ CLI.dumpGenesisData
        putText $ sformat ("System start time is " % shown) $ gdStartTime genesisData
        t <- currentTime
        putText $ sformat ("Current time is " % shown) (Timestamp t)
        currentParams <- getNodeParams cArgs
        putText $ "Wallet is enabled!"
        putText $ sformat ("Using configs and genesis:\n"%shown) conf

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let gtParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        actionWithWallet gtParams currentParams wArgs
  where
    conf = CLI.configurationOptions (CLI.commonArgs cArgs)

main :: IO ()
main = do
    args <- getWalletNodeOptions
    CLI.printFlags
    putText "[Attention] Software is built with wallet part"
    runProduction $ action args
