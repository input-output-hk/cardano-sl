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

import           Control.Concurrent.STM (newTBQueueIO)
import           Data.Maybe (fromJust)
import           Formatting (build, sformat, (%))
import           Mockable (Production, runProduction)
import           System.Wlog (LoggerName, logInfo, modifyLoggerName)

import           Pos.Binary ()
import           Pos.Client.CLI (CommonNodeArgs (..), NodeArgs (..), getNodeParams)
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (OutSpecs)
import           Pos.Communication.Util (ActionSpec (..))
import           Pos.Configuration (walletProductionApi, walletTxCreationDisabled)
import           Pos.Context (HasNodeContext)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Diffusion.Types (Diffusion (..))
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations, NodeParams (..),
                               NodeResources (..), bracketNodeResources, loggerBracket, runNode,
                               withConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util (lensOf, logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (AddrCIdHashes (..), WalletWebMode, bracketWalletWS,
                                 bracketWalletWebDB, getSKById, notifierPlugin, runWRealMode,
                                 startPendingTxsResubmitter, walletServeWebFull, walletServerOuts)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot, cleanupAcidStatePeriodically,
                                       flushWalletStorage, getWalletAddresses)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (processSyncRequest, syncWallet)
import           Pos.Wallet.Web.Tracking.Types (SyncQueue)
import           Pos.Web (serveWeb)
import           Pos.Worker.Types (WorkerSpec, worker)
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
actionWithWallet sscParams nodeParams wArgs@WalletArgs {..} = do
    logInfo "Running `actionWithWallet'"
    bracketWalletWebDB walletDbPath walletRebuildDb $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources nodeParams sscParams
                txpGlobalSettings
                initNodeDBs $ \nr@NodeResources {..} -> do
                ref <- newIORef mempty
                syncRequestsQueue <- liftIO $ newTBQueueIO 50
                runWRealMode
                    db
                    conn
                    (AddrCIdHashes ref)
                    syncRequestsQueue
                    nr
                    (mainAction nr)
  where
    mainAction = runNodeWithInit $ do
        when (walletFlushDb) $ do
            putText "Flushing wallet db..."
            askWalletDB >>= flushWalletStorage
            putText "Resyncing wallets with blockchain..."
            syncWallets
    runNodeWithInit init nr =
        let (ActionSpec f, outs) = runNode nr allPlugins
         in (ActionSpec $ \s -> init >> f s, outs)
    convPlugins = (, mempty) . map (\act -> ActionSpec $ \_ -> act)
    syncWallets :: WalletWebMode ()
    syncWallets = do
      ws  <- askWalletSnapshot
      sks <- mapM getSKById (getWalletAddresses ws)
      forM_ sks (syncWallet . eskToWalletDecrCredentials)
    resubmitterPlugins = ([ActionSpec $ \diffusion -> askWalletDB >>=
                            \db -> startPendingTxsResubmitter db (sendTx diffusion)], mempty)
    notifierPlugins = ([ActionSpec $ \_ -> notifierPlugin], mempty)
    allPlugins :: HasConfigurations => ([WorkerSpec WalletWebMode], OutSpecs)
    allPlugins = mconcat [ convPlugins (plugins wArgs)
                         , walletProd wArgs
                         , acidCleanupWorker wArgs
                         , syncWalletWorker
                         , resubmitterPlugins
                         , notifierPlugins
                         ]

syncWalletWorker :: HasConfigurations => ([WorkerSpec WalletWebMode], OutSpecs)
syncWalletWorker =
    first one $ worker mempty $ const $
    modifyLoggerName (const "syncWalletWorker") $
    (view (lensOf @SyncQueue) >>= processSyncRequest)

acidCleanupWorker :: HasConfigurations => WalletArgs -> ([WorkerSpec WalletWebMode], OutSpecs)
acidCleanupWorker WalletArgs{..} =
    first one $ worker mempty $ const $
    modifyLoggerName (const "acidcleanup") $
    (askWalletDB >>= \db -> cleanupAcidStatePeriodically db walletAcidInterval)

walletProd ::
       ( HasConfigurations
       , HasCompileInfo
       )
    => WalletArgs
    -> ([WorkerSpec WalletWebMode], OutSpecs)
walletProd WalletArgs {..} = first one $ worker walletServerOuts $ \diffusion -> do
    logInfo $ sformat ("Production mode for API: "%build)
        walletProductionApi
    logInfo $ sformat ("Transaction submission disabled: "%build)
        walletTxCreationDisabled
    walletServeWebFull
        diffusion
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
        CLI.printInfoOnStart cArgs
        logInfo $ "Wallet is enabled!"
        currentParams <- getNodeParams loggerName cArgs nodeArgs

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
        logInfo "[Attention] Software is built with wallet part"
        action args
