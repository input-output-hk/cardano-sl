{-# LANGUAGE Rank2Types #-}

module Cardano.Wallet.Launcher
    (
    -- * Arguments and Options
      CommonNodeArgs(..)
    , NodeArgs(..)
    , ConfigurationOptions(..)
    , WalletStartupOptions(..)
    , CommonArgs(..)
    , ChooseWalletBackend(..)
    , WalletBackendParams(..)
    , TlsParams(..)
    , WalletDBOptions(..)

    -- * Launchers
    , startCoreNode
    , startWalletNode

    -- * One-shot Runner
    , runWWebMode
    ) where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Default (Default (..))
import           Ntp.Client (NtpStatus, withNtpClient)
import           System.Wlog (LoggerName (..), Severity (..), logInfo,
                     logMessage, usingLoggerName)

import           Cardano.Wallet.Kernel (PassiveWallet)
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     NewWalletBackendParams (..), WalletBackendParams (..),
                     WalletDBOptions (..), WalletStartupOptions (..),
                     walletDbPath, walletFlushDb, walletRebuildDb)
import           Cardano.Wallet.WalletLayer (PassiveWalletLayer,
                     bracketKernelPassiveWallet)
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Params (getNodeParams, loggingParams)
import           Pos.Client.CLI.Util (printInfoOnStart)
import           Pos.Context (NodeContext (..))
import           Pos.Core.Configuration (epochSlots)
import           Pos.Core.JsonLog (JsonLogConfig (..))
import           Pos.Crypto (ProtocolMagic)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Infra.Ntp.Configuration (NtpConfiguration (..),
                     ntpClientSettings)
import           Pos.Infra.Reporting (noReporter)
import           Pos.Launcher (AssetLockPath (..), BaseParams (..),
                     ConfigurationOptions (..), HasConfigurations,
                     LoggingParams (..), NodeParams (..), NodeResources (..),
                     allocateNodeResources, bracketNodeResources,
                     loggerBracket, releaseNodeResources, runNode, runNodeReal,
                     withConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.Util (logException)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Account (getSKById)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.Server.Launcher (bracketWalletWS,
                     bracketWalletWebDB)
import           Pos.Wallet.Web.Server.Runner (runWRealMode)
import           Pos.Wallet.Web.State (askWalletDB, askWalletSnapshot,
                     flushWalletStorage, getWalletAddresses)
import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.Wallet.Web.Tracking (eskToWalletDecrCredentials,
                     syncWallet)
import           Pos.Web.Types (TlsParams (..))
import           Pos.Worker.Update (updateTriggerWorker)
import           Pos.WorkMode (RealModeContext (..))

import qualified Cardano.Wallet.Kernel.Keystore as Keystore
import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode
import qualified Cardano.Wallet.Server.Plugins as Plugins


-- | start a node from node arguments
startCoreNode
    :: CommonNodeArgs
    -> NodeArgs
    -> LoggerName
    -> IO ()
startCoreNode cArgs nArgs loggerName = do
    let blPath = AssetLockPath <$> cnaAssetLockPath cArgs
    let lparams = loggingParams loggerName cArgs

    loggerBracket lparams . logException loggerName $
        withCompileInfo $ withConfigurations blPath (getConfigOpts cArgs) $ \_ pm -> do
            (params, Just ssc) <- getNodeParams loggerName cArgs nArgs
            runNodeReal pm params ssc [updateTriggerWorker]


-- | Runs an wallet node plus its wallet backend API.
startWalletNode
    :: NodeArgs
    -> WalletStartupOptions
    -> LoggerName
    -> IO ()
startWalletNode nArgs wOpts loggerName = do
    let cArgs  = wsoNodeArgs wOpts
    let blPath = AssetLockPath <$> cnaAssetLockPath cArgs
    let lparams = loggingParams loggerName cArgs

    loggerBracket lparams . logException loggerName $ do
        logInfo "[Attention] Software is built with the wallet backend"
        withCompileInfo $ withConfigurations blPath (getConfigOpts cArgs) $ \ntpConfig pm -> do
            (nodeParams, Just sscParams) <- getNodeParams loggerName cArgs nArgs
            printInfoOnStart cArgs ntpConfig
            logInfo "Wallet is enabled!"
            case wsoWalletBackendParams wOpts of
              WalletLegacy legacyParams ->
                actionWithWallet pm sscParams nodeParams ntpConfig legacyParams
              WalletNew newParams ->
                actionWithNewWallet pm sscParams nodeParams newParams


-- | 'WalletWebMode' headless one-shot runner. If differs from `runWRealMode`
-- by several aspects:
--
--     - It runs the given action and returns, whereas `runWRealMode`
--       actually starts a server waiting for an explicit shutdown.
--
--     - It doesn't run the full diffusion layer, so it's made only for
--       isolate actions on the wallet.
--
--     - It only requires 'external' configurations parameters, as they'd be
--       passed to a node and it handles the internal logic. This makes it much
--       easier to use as one can merely specify a configuration file, a few
--       paths and magic happens.
runWWebMode
    :: forall a
    .  CommonNodeArgs
    -> NodeArgs
    -> WalletBackendParams
    -> LoggerName
    -> ((HasConfigurations, HasCompileInfo) => WalletWebMode a)
    -> IO a
runWWebMode cArgs nArgs wArgs loggerName action = do
    let walletPath = walletDbPath $ walletDbOptions wArgs
    let configOpts = configurationOptions $ commonArgs cArgs
    withCompileInfo $ withConfigurations Nothing configOpts
        $ \_ pm -> bracket (openState True walletPath) closeState
        $ \db -> do
            (params, Just ssc) <- getNodeParams loggerName cArgs nArgs
            let txp     = txpGlobalSettings pm
            let mode    = initNodeDBs pm epochSlots
            let acquire = allocateNodeResources params ssc txp mode
            bracket acquire releaseNodeResources $ \res -> do
                ctx <- WalletWebModeContext
                    <$> pure db
                    <*> newTVarIO def
                    <*> liftIO newTQueueIO
                    <*> pure (nodeResourcesToContext res)

                action `runReaderT` ctx
  where
    nodeResourcesToContext :: NodeResources WalletMempoolExt -> RealModeContext ()
    nodeResourcesToContext NodeResources{..} = RealModeContext
        { rmcNodeDBs       = nrDBs
        , rmcSscState      = nrSscState
        , rmcTxpLocalData  = nrTxpState
        , rmcDelegationVar = nrDlgState
        , rmcJsonLogConfig = JsonLogDisabled
        , rmcLoggerName    = loggerName
        , rmcNodeContext   = nrContext
        , rmcReporter      = noReporter
        }


--
-- INTERNALS
--

getConfigOpts :: CommonNodeArgs -> ConfigurationOptions
getConfigOpts =
    configurationOptions . commonArgs


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
      userSecret <- atomically $ readTVar (ncUserSecret $ nrContext nr)
      liftIO $ Keystore.bracketLegacyKeystore userSecret $ \keystore ->
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
    mainAction = runNodeWithInit

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
