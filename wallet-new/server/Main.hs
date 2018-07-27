{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Maybe (fromJust)
import           Ntp.Client (NtpStatus, withNtpClient)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (ncUserSecret)
import           Pos.Core (epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Ntp.Configuration (NtpConfiguration,
                     ntpClientSettings)
import           Pos.Launcher (NodeParams (..), NodeResources (..),
                     bpLoggingParams, bracketNodeResources, loggerBracket,
                     lpDefaultName, runNode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     ConfigurationOptions, HasConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB, getSKById,
                     getWalletAddresses, runWRealMode)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot,
                     flushWalletStorage)
import           Pos.Wallet.Web.Tracking.Decrypt (eskToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (syncWallet)
import           System.Wlog (LoggerName, Severity (..), logInfo, logMessage,
                     usingLoggerName)

import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode

import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     NewWalletBackendParams (..), WalletBackendParams (..),
                     WalletStartupOptions (..), getWalletNodeOptions,
                     walletDbPath, walletFlushDb, walletRebuildDb)
import qualified Cardano.Wallet.Server.Plugins as Plugins
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer,
                     bracketKernelPassiveWallet)

-- | Default logger name when one is not provided on the command line
defaultLoggerName :: LoggerName
defaultLoggerName = "node"

{-
   Most of the code below has been copied & adapted from wallet/node/Main.hs as a path
   of least resistance to make the wallet-new prototype independent (to an extent)
   from breaking changes to the current wallet.
-}

-- | The "workhorse" responsible for starting a Cardano edge node plus a number of extra plugins.
actionWithWallet :: (HasConfigurations, HasCompileInfo)
                 => ProtocolMagic
                 -> SscParams
                 -> NodeParams
                 -> NtpConfiguration
                 -> WalletBackendParams
                 -> IO ()
actionWithWallet pm sscParams nodeParams ntpConfig wArgs@WalletBackendParams {..} =
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources nodeParams sscParams
                (txpGlobalSettings pm)
                (initNodeDBs pm epochSlots) $ \nr@NodeResources {..} -> do
                    syncQueue <- liftIO newTQueueIO
                    ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
                    runWRealMode pm db conn syncQueue nr (mainAction ntpStatus nr)
  where
    mainAction ntpStatus = runNodeWithInit ntpStatus $ do
        when (walletFlushDb walletDbOptions) $ do
            logInfo "Flushing wallet db..."
            askWalletDB >>= flushWalletStorage
            logInfo "Resyncing wallets with blockchain..."

        -- NOTE(adn): Sync the wallets anyway. The old implementation was skipping syncing in
        -- case `walletFlushDb` was not set, but was still calling it before starting the Servant
        -- server.
        syncWallets

    runNodeWithInit ntpStatus init' nr diffusion = do
        _ <- init'
        runNode pm nr (plugins ntpStatus) diffusion

    syncWallets :: WalletWebMode ()
    syncWallets = do
        addrs <- getWalletAddresses <$> askWalletSnapshot
        sks <- mapM getSKById addrs
        forM_ sks (syncWallet . eskToWalletDecrCredentials)

    plugins :: TVar NtpStatus -> Plugins.Plugin WalletWebMode
    plugins ntpStatus =
        mconcat [ Plugins.conversation wArgs
                , Plugins.legacyWalletBackend pm wArgs ntpStatus
                , Plugins.walletDocumentation wArgs
                , Plugins.acidCleanupWorker wArgs
                , Plugins.syncWalletWorker
                , Plugins.resubmitterPlugin pm
                , Plugins.notifierPlugin
                ]

actionWithNewWallet :: (HasConfigurations, HasCompileInfo)
                    => ProtocolMagic
                    -> SscParams
                    -> NodeParams
                    -> NewWalletBackendParams
                    -> IO ()
actionWithNewWallet pm sscParams nodeParams params =
    bracketNodeResources
        nodeParams
        sscParams
        (txpGlobalSettings pm)
        (initNodeDBs pm epochSlots) $ \nr -> do
      -- TODO: Will probably want to extract some parameters from the
      -- 'NewWalletBackendParams' to construct or initialize the wallet

      -- TODO(ks): Currently using non-implemented layer for wallet layer.
      userSecret <- readTVarIO (ncUserSecret $ nrContext nr)
      liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore -> do
          bracketKernelPassiveWallet logMessage' keystore $ \walletLayer passiveWallet -> do
            liftIO $ logMessage' Info "Wallet kernel initialized"
            Kernel.Mode.runWalletMode pm
                                      nr
                                      walletLayer
                                      (mainAction (walletLayer, passiveWallet) nr)
  where
    mainAction
        :: (PassiveWalletLayer IO, PassiveWallet)
        -> NodeResources ext
        -> (Diffusion Kernel.Mode.WalletMode -> Kernel.Mode.WalletMode ())
    mainAction w nr = runNodeWithInit w nr

    runNodeWithInit
        :: (PassiveWalletLayer IO, PassiveWallet)
        -> NodeResources ext
        -> (Diffusion Kernel.Mode.WalletMode -> Kernel.Mode.WalletMode ())
    runNodeWithInit w nr = runNode pm nr (plugins w)

    -- TODO: Don't know if we need any of the other plugins that are used
    -- in the legacy wallet (see 'actionWithWallet').
    plugins :: (PassiveWalletLayer IO, PassiveWallet)
            -> Plugins.Plugin Kernel.Mode.WalletMode
    plugins w = mconcat [ Plugins.walletBackend pm params w ]

    -- Extract the logger name from node parameters
    --
    -- TODO: Not sure what the policy is for logger names of components.
    -- For now we just use the one from the node itself.
    logMessage' :: Severity -> Text -> IO ()
    logMessage' sev txt =
        usingLoggerName loggerName $ logMessage sev txt
      where
        loggerName :: LoggerName
        loggerName = lpDefaultName . bpLoggingParams . npBaseParams $ nodeParams

-- | Runs an edge node plus its wallet backend API.
startEdgeNode :: HasCompileInfo
              => WalletStartupOptions
              -> IO ()
startEdgeNode wso =
  withConfigurations blPath conf $ \ntpConfig pm -> do
      (sscParams, nodeParams) <- getParameters ntpConfig
      case wsoWalletBackendParams wso of
        WalletLegacy legacyParams ->
          actionWithWallet pm sscParams nodeParams ntpConfig legacyParams
        WalletNew newParams ->
          actionWithNewWallet pm sscParams nodeParams newParams
  where
    getParameters :: HasConfigurations => NtpConfiguration -> IO (SscParams, NodeParams)
    getParameters ntpConfig = do

      currentParams <- CLI.getNodeParams defaultLoggerName (wsoNodeArgs wso) nodeArgs
      let vssSK = fromJust $ npUserSecret currentParams ^. usVss
      let gtParams = CLI.gtSscParams (wsoNodeArgs wso) vssSK (npBehaviorConfig currentParams)

      CLI.printInfoOnStart (wsoNodeArgs wso) ntpConfig
      logInfo "Wallet is enabled!"

      return (gtParams, currentParams)

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs (wsoNodeArgs wso)

    blPath :: Maybe AssetLockPath
    blPath = AssetLockPath <$> CLI.cnaAssetLockPath (wsoNodeArgs wso)

    nodeArgs :: CLI.NodeArgs
    nodeArgs = CLI.NodeArgs { CLI.behaviorConfigPath = Nothing }


-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    cfg <- getWalletNodeOptions
    putText "Wallet is starting..."
    let loggingParams = CLI.loggingParams defaultLoggerName (wsoNodeArgs cfg)
    loggerBracket loggingParams . logException "node" $ do
        logInfo "[Attention] Software is built with the wallet backend"
        startEdgeNode cfg
