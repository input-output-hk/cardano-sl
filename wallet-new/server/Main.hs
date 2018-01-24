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
import           Formatting (sformat, shown, (%))
import           Mockable (Production (..), currentTime, runProduction)
import           Pos.Communication (ActionSpec (..))
import           Pos.Core (Timestamp (..), gdStartTime, genesisData)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (NodeParams (..), NodeResources (..), bracketNodeResources,
                               loggerBracket, runNode, withConfigurations)
import           Pos.Launcher.Configuration (ConfigurationOptions, HasConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB, getSKById, getWalletAddresses,
                                 runWRealMode, syncWalletsWithGState)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (flushWalletStorage)
import           System.Wlog (LoggerName, logInfo)

import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Server.CLI (WalletBackendParams (..), WalletDBOptions (..),
                                            WalletStartupOptions (..), getWalletNodeOptions)
import qualified Cardano.Wallet.Server.Plugins as Plugins
import qualified Pos.Client.CLI as CLI


loggerName :: LoggerName
loggerName = "node"

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
                initNodeDBs $ \nr@NodeResources {..} ->
                runWRealMode db conn nr (mainAction nr)
  where
    mainAction = runNodeWithInit $ do
        when (walletFlushDb walletDbOptions) $ do
            logInfo "Flushing wallet db..."
            flushWalletStorage
            logInfo "Resyncing wallets with blockchain..."
            syncWallets

    runNodeWithInit init nr =
        let (ActionSpec f, outs) = runNode nr plugins
         in (ActionSpec $ \v s -> init >> f v s, outs)

    syncWallets :: WalletWebMode ()
    syncWallets = do
        sks <- getWalletAddresses >>= mapM getSKById
        syncWalletsWithGState sks

    plugins :: HasConfigurations => Plugins.Plugin WalletWebMode
    plugins = mconcat [ Plugins.conversation wArgs
                      , Plugins.walletBackend wArgs
                      , Plugins.acidCleanupWorker wArgs
                      , Plugins.resubmitterPlugin
                      , Plugins.notifierPlugin
                      ]

-- | Runs an edge node plus its wallet backend API.
startEdgeNode :: HasCompileInfo
              => WalletStartupOptions
              -> Production ()
startEdgeNode WalletStartupOptions{..} = do
  withConfigurations conf $ do
    (sscParams, nodeParams) <- getParameters
    actionWithWallet sscParams nodeParams wsoWalletBackendParams
  where
    getParameters :: HasConfigurations => Production (SscParams, NodeParams)
    getParameters = do

      whenJust (CLI.cnaDumpGenesisDataPath wsoNodeArgs) $ CLI.dumpGenesisData True
      t <- currentTime
      currentParams <- CLI.getNodeParams loggerName wsoNodeArgs nodeArgs
      let vssSK = fromJust $ npUserSecret currentParams ^. usVss
      let gtParams = CLI.gtSscParams wsoNodeArgs vssSK (npBehaviorConfig currentParams)

      mapM_ logInfo [
            sformat ("System start time is " % shown) $ gdStartTime genesisData
          , sformat ("Current time is " % shown) (Timestamp t)
          , "Wallet is enabled!"
          , sformat ("Using configs and genesis:\n"%shown) conf
          ]

      return (gtParams, currentParams)

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs wsoNodeArgs

    nodeArgs :: CLI.NodeArgs
    nodeArgs = CLI.NodeArgs { CLI.behaviorConfigPath = Nothing }

-- | Generates the updated spec and store it in the appropriate folder.
-- the reason why we don't generate a yaml file is because for swagger-ui is actually
-- much better to start with the JSON input, as the tool is capable of generating
-- better-looking YAMLs.
generateSwaggerDocumentation :: IO ()
generateSwaggerDocumentation = do
    BL8.writeFile "wallet-new/spec/swagger.json" (encodePretty Swagger.api)
    putText "Swagger API written on disk."

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
  cfg <- getWalletNodeOptions
  putText "Wallet is starting..."
  generateSwaggerDocumentation
  let loggingParams = CLI.loggingParams loggerName (wsoNodeArgs cfg)
  loggerBracket loggingParams . runProduction $ do
    CLI.printFlags
    logInfo "[Attention] Software is built with the wallet backend"
    startEdgeNode cfg
