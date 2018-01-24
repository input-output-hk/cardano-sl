{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

module Main
       ( main
       ) where

import           Universum

import           Data.Maybe (fromJust)
import           Formatting (build, sformat, shown, (%))
import           Mockable (Production, currentTime, runProduction)
import           System.Wlog (LoggerName, logInfo, modifyLoggerName)

import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (ActionSpec (..), OutSpecs, WorkerSpec, worker)
import           Pos.Configuration (walletProductionApi, walletTxCreationDisabled)
import           Pos.Context (HasNodeContext)
import           Pos.Core (Timestamp (..), gdStartTime, genesisData)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, loggerBracket, runNode,
                               withConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (WalletWebMode, bracketWalletWS, bracketWalletWebDB, getSKById,
                                 notifierPlugin, runWRealMode, startPendingTxsResubmitter,
                                 syncWalletsWithGState, walletServeWebFull, walletServerOuts)
import           Pos.Wallet.Web.State (cleanupAcidStatePeriodically, flushWalletStorage,
                                       getWalletAddresses)
import           Pos.Web (serveWeb)
import           Pos.WorkMode (WorkMode)

import           NodeOptions (WalletArgs (..), WalletNodeArgs (..), getWalletNodeOptions)

loggerName :: LoggerName
loggerName = "node"

----------------------------------------------------------------------------
-- Main action
----------------------------------------------------------------------------

actionWithWallet ::
       ( HasConfigurations
       , HasCompileInfo
       )
    => SscParams
    -> NodeParams
    -> WalletArgs
    -> Production ()
actionWithWallet sscParams nodeParams wArgs@WalletArgs {..} =
    bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources nodeParams sscParams
                txpGlobalSettings
                initNodeDBs $ \nr@NodeResources {..} ->
                runWRealMode
                    db
                    conn
                    nr
                    (mainAction nr)
  where
    mainAction = runNodeWithInit $ do
        when (walletFlushDb) $ do
            logInfo "Flushing wallet db..."
            flushWalletStorage
            logInfo "Resyncing wallets with blockchain..."
            syncWallets
    runNodeWithInit init nr =
        let (ActionSpec f, outs) = runNode nr allPlugins
         in (ActionSpec $ \v s -> init >> f v s, outs)
    convPlugins = (, mempty) . map (\act -> ActionSpec $ \__vI __sA -> act)
    syncWallets :: WalletWebMode ()
    syncWallets = do
        sks <- getWalletAddresses >>= mapM getSKById
        syncWalletsWithGState sks
    resubmitterPlugins = ([ActionSpec $ \_ _ -> startPendingTxsResubmitter], mempty)
    notifierPlugins = ([ActionSpec $ \_ _ -> notifierPlugin], mempty)
    allPlugins :: HasConfigurations => ([WorkerSpec WalletWebMode], OutSpecs)
    allPlugins = mconcat [ convPlugins (plugins wArgs)
                         , walletProd wArgs
                         , acidCleanupWorker wArgs
                         , resubmitterPlugins
                         , notifierPlugins
                         ]

acidCleanupWorker :: HasConfigurations => WalletArgs -> ([WorkerSpec WalletWebMode], OutSpecs)
acidCleanupWorker WalletArgs{..} =
    first one $ worker mempty $ const $
    modifyLoggerName (const "acidcleanup") $
    cleanupAcidStatePeriodically walletAcidInterval

walletProd ::
       ( HasConfigurations
       , HasCompileInfo
       )
    => WalletArgs
    -> ([WorkerSpec WalletWebMode], OutSpecs)
walletProd WalletArgs {..} = first one $ worker walletServerOuts $ \sendActions -> do
    logInfo $ sformat ("Production mode for API: "%build)
        walletProductionApi
    logInfo $ sformat ("Transaction submission disabled: "%build)
        walletTxCreationDisabled

    walletServeWebFull
        sendActions
        walletDebug
        walletAddress
        (Just walletTLSParams)

plugins ::
    ( WorkMode ctx m
    , HasNodeContext ctx
    , HasConfigurations
    , HasCompileInfo
    ) => WalletArgs -> [m ()]
plugins WalletArgs {..}
    | enableWeb = [serveWeb webPort (Just walletTLSParams)]
    | otherwise = []

action :: HasCompileInfo => WalletNodeArgs -> Production ()
action (WalletNodeArgs (cArgs@CommonNodeArgs{..}) (wArgs@WalletArgs{..})) =
    withConfigurations conf $ do
        whenJust cnaDumpGenesisDataPath $ CLI.dumpGenesisData True
        logInfo $ sformat ("System start time is " % shown) $ gdStartTime genesisData
        t <- currentTime
        logInfo $ sformat ("Current time is " % shown) (Timestamp t)
        currentParams <- getNodeParams loggerName cArgs nodeArgs
        logInfo $ "Wallet is enabled!"
        logInfo $ sformat ("Using configs and genesis:\n"%shown) conf

        let vssSK = fromJust $ npUserSecret currentParams ^. usVss
        let sscParams = CLI.gtSscParams cArgs vssSK (npBehaviorConfig currentParams)

        actionWithWallet sscParams currentParams wArgs
  where
    nodeArgs :: NodeArgs
    nodeArgs = NodeArgs { behaviorConfigPath = Nothing }

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs cArgs


main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
    args <- getWalletNodeOptions
    let loggingParams = CLI.loggingParams loggerName (wnaCommonNodeArgs args)
    loggerBracket loggingParams . logException "node" . runProduction $ do
        CLI.printFlags
        logInfo "[Attention] Software is built with wallet part"
        action args
