{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
module Main where

import           Prelude
import           CLI
import           Types
import           Rendering                   (say, bold)
import           Control.Lens
import           Control.Monad.Reader
import           Serokell.Util               (sec)
import           Data.Text                   (pack)
import           Data.Default                (def)
import           Data.Maybe                  (fromJust, isJust)
import           Data.IORef                  (newIORef)
import           GHC.Conc
import           Lib
import           Mockable                    (Production, runProduction)
import           Options.Generic
import           Pos.Client.CLI              (CommonArgs (..), CommonNodeArgs (..),
                                              NodeArgs (..), getNodeParams, gtSscParams)
import           Pos.Core                    (Timestamp (..))
import           Pos.DB.Rocks.Functions
import           Pos.DB.Rocks.Types
import           Pos.Launcher
import           Pos.Network.CLI
import           Pos.Network.Types
import           Pos.Ssc.SscAlgo
import           Pos.Util.JsonLog
import           Pos.Util.UserSecret         (usVss)
import           Pos.Wallet.SscType          (WalletSscType)
import           Pos.Wallet.Web.Mode
import           Pos.Wallet.Web.State.Acidic
import           Pos.Wallet.Web.State.State  (WalletState)
import           Pos.WorkMode
import           Stats                       (showStatsAndExit, showStatsData)
import           System.IO
import           System.Wlog.LoggerName
import           System.Wlog.LoggerNameBox

newRealModeContext
    :: HasConfigurations
    => NodeDBs
    -> ConfigurationOptions
    -> FilePath
    -> Production (RealModeContext WalletSscType)
newRealModeContext dbs confOpts secretKeyPath = do
    let nodeArgs = NodeArgs {
      sscAlgo            = GodTossingAlgo
    , behaviorConfigPath = Nothing
    }
    let networkOps = NetworkConfigOpts {
          networkConfigOptsTopology = Nothing
        , networkConfigOptsKademlia = Nothing
        , networkConfigOptsSelf     = Nothing
        , networkConfigOptsPort     = 3030
        , networkConfigOptsPolicies = Nothing
        }
    let cArgs@CommonNodeArgs {..} = CommonNodeArgs {
           dbPath                 = "node-db"
         , rebuildDB              = True
         , devSpendingGenesisI    = Nothing
         , devVssGenesisI         = Nothing
         , keyfilePath            = secretKeyPath
         , backupPhrase           = Nothing
         , externalAddress        = Nothing
         , bindAddress            = Nothing
         , peers                  = mempty
         , networkConfigOpts      = networkOps
         , jlPath                 = Nothing
         , kademliaDumpPath       = "kademlia.dump"
         , commonArgs             = CommonArgs {
               logConfig            = Nothing
             , logPrefix            = Nothing
             , reportServers        = mempty
             , updateServers        = mempty
             , configurationOptions = confOpts
             }
         , updateLatestPath       = "update"
         , updateWithPackage      = False
         , noNTP                  = True
         , route53Params          = Nothing
         , enableMetrics          = False
         , ekgParams              = Nothing
         , statsdParams           = Nothing
         , cnaDumpGenesisDataPath = Nothing
         }
    nodeParams <- getNodeParams cArgs nodeArgs
    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @WalletSscType @IO nodeParams gtParams $ \NodeResources{..} ->
        RealModeContext <$> pure dbs
                        <*> pure nrSscState
                        <*> pure nrTxpState
                        <*> pure nrDlgState
                        <*> jsonLogConfigFromHandle stdout
                        <*> pure (LoggerName "dbgen")
                        <*> pure nrContext
                        <*> initQueue (defaultNetworkConfig (TopologyAuxx mempty)) Nothing


walletRunner
    :: HasConfigurations
    => ConfigurationOptions
    -> NodeDBs
    -> FilePath
    -> WalletState
    -> UberMonad a
    -> IO a
walletRunner confOpts dbs secretKeyPath ws act = runProduction $ do
    wwmc <- WalletWebModeContext <$> pure ws
                                 <*> liftIO (newTVarIO def)
                                 <*> liftIO (AddrCIdHashes <$> (newIORef mempty))
                                 <*> newRealModeContext dbs confOpts secretKeyPath
    runReaderT act wwmc

newWalletState :: (MonadIO m, HasConfigurations) => Bool -> FilePath -> m WalletState
newWalletState recreate walletPath =
    -- If the user passed the `--add-to` option, it means we don't have
    -- to rebuild the DB, but rather append stuff into it.
    liftIO $ openState (not recreate) walletPath

instance HasLoggerName IO where
    getLoggerName = pure $ LoggerName "dbgen"
    modifyLoggerName _ x = x

-- TODO(ks): Fix according to Pos.Client.CLI.Options
newConfig :: CLI -> ConfigurationOptions
newConfig CLI{..} = defaultConfigurationOptions {
      cfoSystemStart  = Timestamp . sec <$> systemStart
    , cfoFilePath     = configurationPath
    , cfoKey          = pack configurationProf
    }

-- stack exec dbgen -- --config ./tools/src/dbgen/config.dhall --nodeDB db-mainnet --walletDB wdb-mainnet --configPath node/configuration.yaml --secretKey secret-mainnet.key --configProf mainnet_full
main :: IO ()
main = do

    cli@CLI{..} <- getRecord "DBGen"
    let cfg = newConfig cli

    withConfigurations cfg $ do
        when showStats (showStatsAndExit walletPath)

        say $ bold "Starting the modification of the wallet..."

        showStatsData "before" walletPath

        dbs  <- openNodeDBs False nodePath -- Do not recreate!
        spec <- loadGenSpec config
        ws   <- newWalletState (isJust addTo) walletPath -- Recreate or not

        let generatedWallet = generate cli spec
        walletRunner cfg dbs secretKeyPath ws generatedWallet
        closeState ws

        showStatsData "after" walletPath
