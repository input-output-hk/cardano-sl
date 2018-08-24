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
import           Ntp.Client (NtpConfiguration, NtpStatus, ntpClientSettings,
                     withNtpClient)
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import qualified Pos.Client.CLI as CLI
import           Pos.Context (ncUserSecret)
import           Pos.Core (epochSlots)
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (NodeParams (..), NodeResources (..),
                     bpLoggingParams, bracketNodeResources, loggerBracket,
                     lpDefaultName, runNode, withConfigurations)
import           Pos.Launcher.Configuration (AssetLockPath (..),
                     ConfigurationOptions, HasConfigurations)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB,
                     getKeyById, getWalletAddresses, runWRealMode)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot,
                     flushWalletStorage)
import           Pos.Wallet.Web.Tracking.Decrypt (keyToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (syncWallet)
import           System.Wlog (LoggerName, Severity (..), logInfo, logMessage,
                     usingLoggerName)

import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode

import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as NodeStateAdaptor
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     NewWalletBackendParams (..), WalletBackendParams (..),
                     WalletStartupOptions (..), getWalletNodeOptions,
                     walletDbPath, walletFlushDb, walletRebuildDb)
import qualified Cardano.Wallet.Server.LegacyPlugins as LegacyPlugins
import qualified Cardano.Wallet.Server.Plugins as Plugins
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel

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
                 -> TxpConfiguration
                 -> SscParams
                 -> NodeParams
                 -> NtpConfiguration
                 -> WalletBackendParams
                 -> IO ()
actionWithWallet pm txpConfig sscParams nodeParams ntpConfig wArgs@WalletBackendParams {..} =
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources nodeParams sscParams
                (txpGlobalSettings pm txpConfig)
                (initNodeDBs pm epochSlots) $ \nr@NodeResources {..} -> do
                    syncQueue <- liftIO newTQueueIO
                    ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
                    runWRealMode pm txpConfig db conn syncQueue nr (mainAction ntpStatus nr)
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
        runNode pm txpConfig nr (plugins ntpStatus) diffusion

    syncWallets :: WalletWebMode ()
    syncWallets = do
        addrs <- getWalletAddresses <$> askWalletSnapshot
        keys' <- mapM getKeyById addrs
        forM_ keys' (syncWallet . keyToWalletDecrCredentials)

    plugins :: TVar NtpStatus -> LegacyPlugins.Plugin WalletWebMode
    plugins ntpStatus =
        mconcat [ LegacyPlugins.conversation wArgs
                , LegacyPlugins.legacyWalletBackend pm txpConfig wArgs ntpStatus
                , LegacyPlugins.walletDocumentation wArgs
                , LegacyPlugins.acidCleanupWorker wArgs
                , LegacyPlugins.syncWalletWorker
                , LegacyPlugins.resubmitterPlugin pm txpConfig
                , LegacyPlugins.notifierPlugin
                ]

actionWithNewWallet :: (HasConfigurations, HasCompileInfo)
                    => ProtocolMagic
                    -> TxpConfiguration
                    -> SscParams
                    -> NodeParams
                    -> NtpConfiguration
                    -> NewWalletBackendParams
                    -> IO ()
actionWithNewWallet pm txpConfig sscParams nodeParams ntpConfig params =
    bracketNodeResources
        nodeParams
        sscParams
        (txpGlobalSettings pm txpConfig)
        (initNodeDBs pm epochSlots) $ \nr -> do
      ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
      userSecret <- readTVarIO (ncUserSecret $ nrContext nr)
      let nodeState = NodeStateAdaptor.newNodeStateAdaptor nr ntpStatus
      liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore -> do
          WalletLayer.Kernel.bracketPassiveWallet logMessage' keystore nodeState $ \walletLayer passiveWallet -> do
            Kernel.init passiveWallet
            Kernel.Mode.runWalletMode pm
                                      txpConfig
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
    runNodeWithInit w nr = runNode pm txpConfig nr (plugins w)

    -- FIXME: Do we need the monitoring API ?
    plugins :: (PassiveWalletLayer IO, PassiveWallet)
            -> Plugins.Plugin Kernel.Mode.WalletMode
    plugins w = mconcat
        -- The actual wallet backend server.
        [ Plugins.apiServer pm params w

        -- The corresponding wallet documention, served as a different
        -- server which doesn't require client x509 certificates to
        -- connect, but still serves the doc through TLS
        , Plugins.docServer params

        -- Periodically compact & snapshot the acid-state database.
        , Plugins.acidStateSnapshots

        -- | A @Plugin@ to notify frontend via websockets.
        , Plugins.updateNotifier
        ]

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
  withConfigurations blPath conf $ \pm txpConfig ntpConfig -> do
      (sscParams, nodeParams) <- getParameters txpConfig ntpConfig
      case wsoWalletBackendParams wso of
        WalletLegacy legacyParams ->
          actionWithWallet pm txpConfig sscParams nodeParams ntpConfig legacyParams
        WalletNew newParams ->
          actionWithNewWallet pm txpConfig sscParams nodeParams ntpConfig newParams
  where
    getParameters :: HasConfigurations
                  => TxpConfiguration
                  -> NtpConfiguration
                  -> IO (SscParams, NodeParams)
    getParameters txpConfig ntpConfig = do

      currentParams <- CLI.getNodeParams defaultLoggerName (wsoNodeArgs wso) nodeArgs
      let vssSK = fromJust $ npUserSecret currentParams ^. usVss
      let gtParams = CLI.gtSscParams (wsoNodeArgs wso) vssSK (npBehaviorConfig currentParams)

      CLI.printInfoOnStart (wsoNodeArgs wso) ntpConfig txpConfig
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
