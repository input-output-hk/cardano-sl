module Cardano.Wallet.LegacyAction(actionWithWallet) where

import           Universum

import           Control.Concurrent.STM (newTBQueueIO)

import           Ntp.Client (NtpConfiguration, NtpStatus, ntpClientSettings,
                     withNtpClient)
import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Core.NetworkMagic (NetworkMagic, makeNetworkMagic)
import           Pos.Launcher (NodeParams (..), NodeResources (..),
                     WalletConfiguration (..), runNode)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Wlog (logInfo)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB,
                     getKeyById, getWalletAddresses, runWRealMode)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot,
                     flushWalletStorage)
import           Pos.Wallet.Web.Tracking.Decrypt (keyToWalletDecrCredentials)
import           Pos.Wallet.Web.Tracking.Sync (syncWallet)
import           Pos.WorkMode (EmptyMempoolExt)

import qualified Cardano.Wallet.API.V1.Headers as Headers
import           Cardano.Wallet.Server.CLI (WalletBackendParams (..),
                     walletDbPath, walletFlushDb, walletRebuildDb)
import qualified Cardano.Wallet.Server.LegacyPlugins as LegacyPlugins
import           Cardano.Wallet.Server.Middlewares (throttleMiddleware,
                     withDefaultHeader)


-- | The legacy function responsible for starting a Cardano edge node plus a
-- number of extra plugins.
actionWithWallet
    :: (HasConfigurations, HasCompileInfo)
    => WalletBackendParams
    -> Genesis.Config
    -> WalletConfiguration
    -> TxpConfiguration
    -> NtpConfiguration
    -> NodeParams
    -> SscParams
    -> NodeResources EmptyMempoolExt
    -> IO ()
actionWithWallet wArgs@WalletBackendParams {..} genesisConfig walletConfig txpConfig ntpConfig _ _ nodeRes = do
    logInfo "[Attention] Software is built with the wallet backend"
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn -> do
            syncQueue <- liftIO $ newTBQueueIO 64
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

    nm :: NetworkMagic
    nm = makeNetworkMagic $ configProtocolMagic genesisConfig

    syncWallets :: WalletWebMode ()
    syncWallets = do
        addrs <- getWalletAddresses <$> askWalletSnapshot
        keys' <- mapM (getKeyById nm) addrs
        forM_ keys' (syncWallet . keyToWalletDecrCredentials nm)

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
