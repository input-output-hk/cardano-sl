module Cardano.Wallet.Action (actionWithWallet) where

import           Universum

import           Ntp.Client (NtpConfiguration, ntpClientSettings, withNtpClient)

import           Pos.Chain.Genesis as Genesis (Config (..))
import           Pos.Chain.Ssc (SscParams)
import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Context (ncUserSecret)
import           Pos.Launcher (NodeParams (..), NodeResources (..),
                     WalletConfiguration (..), bpLoggingParams, lpDefaultName,
                     runNode)
import           Pos.Launcher.Configuration (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Util.Wlog (LoggerName, Severity (..), logInfo, logMessage,
                     usingLoggerName)
import           Pos.WorkMode (EmptyMempoolExt)

import qualified Cardano.Wallet.API.V1.Headers as Headers
import           Cardano.Wallet.Kernel (PassiveWallet)
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Internal as Kernel.Internal
import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import           Cardano.Wallet.Kernel.Migration (migrateLegacyDataLayer)
import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode
import qualified Cardano.Wallet.Kernel.NodeStateAdaptor as NodeStateAdaptor
import           Cardano.Wallet.Kernel.ProtocolParameters
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams,
                     getFullMigrationFlag, getWalletDbOptions, walletDbPath,
                     walletRebuildDb)
import           Cardano.Wallet.Server.Middlewares
                     (faultInjectionHandleIgnoreAPI, throttleMiddleware,
                     unsupportedMimeTypeMiddleware, withDefaultHeader)
import qualified Cardano.Wallet.Server.Plugins as Plugins
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer)
import qualified Cardano.Wallet.WalletLayer.Kernel as WalletLayer.Kernel


-- | The "workhorse" responsible for starting a Cardano edge node plus a number of extra plugins.
actionWithWallet
    :: (HasConfigurations, HasCompileInfo)
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

    (nodeClient, _) <- setupClient params
    let protocolParams = newProtocolParameterAdaptor nodeClient
    liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore -> do
        let dbOptions = getWalletDbOptions params
        let dbPath = walletDbPath dbOptions
        let rebuildDB = walletRebuildDb dbOptions
        let dbMode = Kernel.UseFilePath (Kernel.DatabaseOptions {
              Kernel.dbPathAcidState = dbPath <> "-acid"
            , Kernel.dbPathMetadata  = dbPath <> "-sqlite.sqlite3"
            , Kernel.dbRebuild       = rebuildDB
            })
        let pm = configProtocolMagic genesisConfig
        WalletLayer.Kernel.bracketPassiveWallet pm dbMode logMessage' keystore nodeState protocolParams (npFInjects nodeParams) $ \walletLayer passiveWallet -> do
            migrateLegacyDataLayer passiveWallet dbPath (getFullMigrationFlag params)

            let plugs = plugins (walletLayer, passiveWallet) dbMode

            Kernel.Mode.runWalletMode
                genesisConfig
                txpConfig
                nodeRes
                walletLayer
                (runNode genesisConfig txpConfig nodeRes plugs)
  where
    plugins :: (PassiveWalletLayer IO, PassiveWallet)
            -> Kernel.DatabaseMode
            -> [ (Text, Plugins.Plugin Kernel.Mode.WalletMode) ]
    plugins w dbMode = concat [
            -- The actual wallet backend server.
            [
              ("wallet-new api worker", Plugins.apiServer params w
                [ faultInjectionHandleIgnoreAPI (npFInjects nodeParams) -- This allows dynamic control of fault injection
                , throttleMiddleware (ccThrottle walletConfig)          -- Throttle requests
                , withDefaultHeader Headers.applicationJson
                , unsupportedMimeTypeMiddleware
                ])

            -- Periodically compact & snapshot the acid-state database.
            , ("acid state cleanup", Plugins.acidStateSnapshots (view Kernel.Internal.wallets (snd w)) params dbMode)

            -- A @Plugin@ to watch and store incoming update proposals
            , ("update watcher", Plugins.updateWatcher)
            ]
        -- The corresponding wallet documention, served as a different
        -- server which doesn't require client x509 certificates to
        -- connect, but still serves the doc through TLS
        , maybe [] (pure . ("doc server",)) (Plugins.docServer params)
        -- The monitoring API for the Core node.
        , Plugins.monitoringServer params
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
