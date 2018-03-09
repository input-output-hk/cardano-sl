{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe (fromJust)
import           Mockable (Production (..), runProduction)
import qualified Pos.Client.CLI as CLI
import           Pos.Communication (ActionSpec (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (NodeParams (..), NodeResources (..), bpLoggingParams,
                               bracketNodeResources, loggerBracket, lpDefaultName, runNode,
                               withConfigurations)
import           Pos.Launcher.Configuration (ConfigurationOptions, HasConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util (logException)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (AddrCIdHashes (..), bracketWalletWS, bracketWalletWebDB, getSKById,
                                 getWalletAddresses, runWRealMode, syncWalletsWithGState)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (flushWalletStorage)
import           Servant.Swagger (HasSwagger)
import           System.Wlog (LoggerName, Severity, logInfo, logMessage, usingLoggerName)

import           Cardano.Wallet.API (walletAPI)
import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import qualified Cardano.Wallet.Kernel as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..), NewWalletBackendParams (..),
                                            WalletBackendParams (..), WalletStartupOptions (..),
                                            getWalletNodeOptions, isDebugMode, walletDbPath,
                                            walletFlushDb, walletRebuildDb)
import qualified Cardano.Wallet.Server.Plugins as Plugins

import           Cardano.Wallet.API.V1.Swagger ()

-- | Default logger name when one is not provided on the command line
defaultLoggerName :: LoggerName
defaultLoggerName = "node"

{-
   Most of the code below has been copied & adapted from wallet/node/Main.hs as a path
   of least resistance to make the wallet-new prototype independent (to an extend)
   from breaking changes to the current wallet.
-}

-- | The "workhorse" responsible for starting a Cardano edge node plus a number of extra plugins.
actionWithWallet :: (HasConfigurations, HasCompileInfo)
                 => SscParams
                 -> NodeParams
                 -> WalletBackendParams
                 -> Production ()
actionWithWallet sscParams nodeParams wArgs@WalletBackendParams {..} =
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources nodeParams sscParams
                txpGlobalSettings
                initNodeDBs $ \nr@NodeResources {..} -> do
                    ref <- newIORef mempty
                    runWRealMode db conn (AddrCIdHashes ref) nr (mainAction nr)
  where
    mainAction = runNodeWithInit $ do
        when (walletFlushDb walletDbOptions) $ do
            logInfo "Flushing wallet db..."
            flushWalletStorage
            logInfo "Resyncing wallets with blockchain..."
            syncWallets

    runNodeWithInit init nr =
        let (ActionSpec f, outs) = runNode nr plugins
         in (ActionSpec $ \s -> init >> f s, outs)

    syncWallets :: WalletWebMode ()
    syncWallets = do
        sks <- getWalletAddresses >>= mapM getSKById
        syncWalletsWithGState sks

    plugins :: HasConfigurations => Plugins.Plugin WalletWebMode
    plugins = mconcat [ Plugins.conversation wArgs
                      , Plugins.legacyWalletBackend wArgs
                      , Plugins.acidCleanupWorker wArgs
                      , Plugins.resubmitterPlugin
                      , Plugins.notifierPlugin
                      ]

actionWithNewWallet :: (HasConfigurations, HasCompileInfo)
                    => SscParams
                    -> NodeParams
                    -> NewWalletBackendParams
                    -> Production ()
actionWithNewWallet sscParams nodeParams params =
    bracketNodeResources
        nodeParams
        sscParams
        txpGlobalSettings
        initNodeDBs $ \nr -> do
      -- TODO: Will probably want to extract some parameters from the
      -- 'NewWalletBackendParams' to construct or initialize the wallet
      Kernel.bracketPassiveWallet logMessage' $ \wallet ->
        Kernel.Mode.runWalletMode nr wallet (mainAction wallet nr)
  where
    mainAction w = runNodeWithInit w $
        liftIO $ Kernel.init w

    runNodeWithInit w init nr =
        let (ActionSpec f, outs) = runNode nr (plugins w)
         in (ActionSpec $ \s -> init >> f s, outs)

    -- TODO: Don't know if we need any of the other plugins that are used
    -- in the legacy wallet (see 'actionWithWallet').
    plugins :: Kernel.PassiveWallet -> Plugins.Plugin Kernel.Mode.WalletMode
    plugins w = mconcat [ Plugins.walletBackend params w ]

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
              -> Production ()
startEdgeNode WalletStartupOptions{..} =
  withConfigurations conf $ do
      (sscParams, nodeParams) <- getParameters
      case wsoWalletBackendParams of
        WalletLegacy legacyParams -> do
          when (isDebugMode $ walletRunMode legacyParams) $
              generateSwaggerDocumentation walletAPI
          actionWithWallet sscParams nodeParams legacyParams
        WalletNew newParams ->
          actionWithNewWallet sscParams nodeParams newParams
  where
    getParameters :: HasConfigurations => Production (SscParams, NodeParams)
    getParameters = do

      currentParams <- CLI.getNodeParams defaultLoggerName wsoNodeArgs nodeArgs
      let vssSK = fromJust $ npUserSecret currentParams ^. usVss
      let gtParams = CLI.gtSscParams wsoNodeArgs vssSK (npBehaviorConfig currentParams)

      CLI.printInfoOnStart wsoNodeArgs
      logInfo "Wallet is enabled!"

      return (gtParams, currentParams)

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs wsoNodeArgs

    nodeArgs :: CLI.NodeArgs
    nodeArgs = CLI.NodeArgs { CLI.behaviorConfigPath = Nothing }

-- | Generates the updated spec and store it in the appropriate folder.
-- the reason why we don't generate a yaml file is because for swagger-ui is actually
-- much better to start with the JSON input, as the tool is capable of generating
-- better-looking YAMLs.
generateSwaggerDocumentation :: ( MonadIO m
                                , HasCompileInfo
                                , HasUpdateConfiguration
                                , HasSwagger a
                                )
                             => Proxy a
                             -> m ()
generateSwaggerDocumentation api = liftIO $ do
    BL8.writeFile "wallet-new/spec/swagger.json" (encodePretty $ Swagger.api api)
    putText "Swagger API written on disk."

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
    cfg <- getWalletNodeOptions
    putText "Wallet is starting..."
    let loggingParams = CLI.loggingParams defaultLoggerName (wsoNodeArgs cfg)
    loggerBracket loggingParams . logException "node" . runProduction $ do
        logInfo "[Attention] Software is built with the wallet backend"
        startEdgeNode cfg
