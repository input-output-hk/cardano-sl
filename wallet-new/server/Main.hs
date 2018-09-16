{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)

import           Ntp.Client (NtpConfiguration, NtpStatus, ntpClientSettings,
                     withNtpClient)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI (NodeArgs (..), loggingParams)
import           Pos.Context (ncUserSecret)
import           Pos.Launcher (NodeParams (..), NodeResources (..),
                     WalletConfiguration (..), bpLoggingParams, launchNode,
                     lpDefaultName, runNode)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Wlog (LoggerName, Severity (..), logInfo, logMessage,
                     usingLoggerName)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB,
                     getKeyById, getWalletAddresses, runWRealMode)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot,
                     flushWalletStorage)
import           Pos.Wallet.Web.Tracking.Decrypt (keyToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (syncWallet)
import           Pos.WorkMode (EmptyMempoolExt)

import qualified Cardano.Wallet.API.V1.Headers as Headers
import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Internal as Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Migration (migrateLegacyDataLayer)
import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as NodeStateAdaptor
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     NewWalletBackendParams, WalletBackendParams (..),
                     WalletStartupOptions (..), getWalletDbOptions,
                     getWalletNodeOptions, walletDbPath, walletFlushDb,
                     walletRebuildDb)
import qualified Cardano.Wallet.Server.LegacyPlugins as LegacyPlugins
import           Cardano.Wallet.Server.Middlewares (throttleMiddleware,
                     withDefaultHeader)
import qualified Cardano.Wallet.Server.Plugins as Plugins
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel


-- | The legacy function responsible for starting a Cardano edge node plus a
-- number of extra plugins.
actionWithLegacyWallet :: (HasConfigurations, HasCompileInfo)
                 => WalletBackendParams
                 -> Genesis.Config
                 -> WalletConfiguration
                 -> TxpConfiguration
                 -> NtpConfiguration
                 -> NodeParams
                 -> SscParams
                 -> NodeResources EmptyMempoolExt
                 -> IO ()
actionWithLegacyWallet wArgs@WalletBackendParams {..} genesisConfig walletConfig txpConfig ntpConfig _ _ nodeRes = do
    logInfo "[Attention] Software is built with the wallet backend"
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn -> do
            syncQueue <- liftIO newTQueueIO
            ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
            runWRealMode genesisConfig txpConfig db conn syncQueue nodeRes (mainAction ntpStatus)
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

    runNodeWithInit ntpStatus init' diffusion = do
        _ <- init'
        runNode genesisConfig txpConfig nodeRes (plugins ntpStatus) diffusion

    syncWallets :: WalletWebMode ()
    syncWallets = do
        addrs <- getWalletAddresses <$> askWalletSnapshot
        keys' <- mapM getKeyById addrs
        forM_ keys' (syncWallet . keyToWalletDecrCredentials)

    plugins :: TVar NtpStatus -> LegacyPlugins.Plugin WalletWebMode
    plugins ntpStatus =
        mconcat [ LegacyPlugins.conversation wArgs
                , LegacyPlugins.legacyWalletBackend genesisConfig txpConfig wArgs ntpStatus
                    [ throttleMiddleware (ccThrottle walletConfig)
                    , withDefaultHeader Headers.applicationJson
                    ]
                , LegacyPlugins.walletDocumentation wArgs
                , LegacyPlugins.acidCleanupWorker wArgs
                , LegacyPlugins.syncWalletWorker genesisConfig
                , LegacyPlugins.resubmitterPlugin genesisConfig txpConfig
                , LegacyPlugins.notifierPlugin
                ]


-- | The "workhorse" responsible for starting a Cardano edge node plus a number of extra plugins.
actionWithWallet :: (HasConfigurations, HasCompileInfo)
                 => NewWalletBackendParams
                 -> Genesis.Config
                 -> WalletConfiguration
                 -> TxpConfiguration
                 -> NtpConfiguration
                 -> NodeParams
                 -> SscParams
                 -> NodeResources EmptyMempoolExt
                 -> IO ()
actionWithWallet params genesisConfig walletConfig txpConfig ntpConfig nodeParams _ nodeRes = do
    logInfo "[Attention] Software is built with the wallet backend"
    ntpStatus <- withNtpClient (ntpClientSettings ntpConfig)
    userSecret <- readTVarIO (ncUserSecret $ nrContext nodeRes)
    let nodeState = NodeStateAdaptor.newNodeStateAdaptor
            genesisConfig
            nodeRes
            ntpStatus
    liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore -> do
        let dbPath = walletDbPath (getWalletDbOptions params)
        let dbMode = Kernel.UseFilePath (Kernel.DatabasePaths {
              Kernel.dbPathAcidState = dbPath <> "-acid"
            , Kernel.dbPathMetadata  = dbPath <> "-sqlite.sqlite3"
            })
        WalletLayer.Kernel.bracketPassiveWallet dbMode logMessage' keystore nodeState $ \walletLayer passiveWallet -> do
            migrateLegacyDataLayer passiveWallet dbPath

            let plugs = plugins (walletLayer, passiveWallet) dbMode

            Kernel.Mode.runWalletMode
                genesisConfig
                txpConfig
                nodeRes
                walletLayer
                (runNode genesisConfig txpConfig nodeRes plugs)
  where
    pm = configProtocolMagic genesisConfig

    plugins :: (PassiveWalletLayer IO, PassiveWallet)
            -> Kernel.DatabaseMode
            -> Plugins.Plugin Kernel.Mode.WalletMode
    plugins w dbMode = mconcat
        -- The actual wallet backend server.
        [ Plugins.apiServer pm params w
            -- Throttle requests.
            [ throttleMiddleware (ccThrottle walletConfig)
            , withDefaultHeader Headers.applicationJson
            ]

        -- The corresponding wallet documention, served as a different
        -- server which doesn't require client x509 certificates to
        -- connect, but still serves the doc through TLS
        , Plugins.docServer params

        -- The monitoring API for the Core node.
        , Plugins.monitoringServer params

        -- Periodically compact & snapshot the acid-state database.
        , Plugins.acidStateSnapshots (view Kernel.Internal.wallets (snd w)) params dbMode

        -- | A @Plugin@ to watch and store incoming update proposals
        , Plugins.updateWatcher
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


-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $ do
    WalletStartupOptions cArgs wArgs <- getWalletNodeOptions
    let lArgs = loggingParams "node" cArgs
    let nArgs = NodeArgs { behaviorConfigPath = Nothing }
    putText "Wallet is starting..."

    launchNode nArgs cArgs lArgs $ case wArgs of
        WalletLegacy p ->
            actionWithLegacyWallet p

        WalletNew p ->
            actionWithWallet p
