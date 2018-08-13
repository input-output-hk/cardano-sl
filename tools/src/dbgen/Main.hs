{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TypeApplications    #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Default (def)
import           Data.Maybe (fromJust, isJust)
import           Data.Time.Units (fromMicroseconds)
import qualified Network.Transport.TCP as TCP
import           Options.Generic (getRecord)

import           Pos.Chain.Txp (TxpConfiguration)
import           Pos.Client.CLI (CommonArgs (..), CommonNodeArgs (..),
                     NodeArgs (..), getNodeParams, gtSscParams)
import           Pos.Core (ProtocolMagic, Timestamp (..), epochSlots)
import           Pos.DB.DB (initNodeDBs)
import           Pos.DB.Rocks.Functions (openNodeDBs)
import           Pos.DB.Rocks.Types (NodeDBs)
import           Pos.DB.Txp (txpGlobalSettings)
import           Pos.Infra.Network.CLI (NetworkConfigOpts (..))
import           Pos.Infra.Network.Types (NetworkConfig (..), Topology (..),
                     topologyDequeuePolicy, topologyEnqueuePolicy,
                     topologyFailurePolicy)
import           Pos.Infra.Reporting (noReporter)
import           Pos.Infra.Util.JsonLog.Events (jsonLogConfigFromHandle)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeResources (..), bracketNodeResources,
                     defaultConfigurationOptions, npBehaviorConfig,
                     npUserSecret, withConfigurations)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web.Mode (WalletWebModeContext (..))
import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.Wallet.Web.State.State (WalletDB)
import           Pos.WorkMode (RealModeContext (..))
import           System.Wlog (HasLoggerName (..), LoggerName (..))

import           Pos.Tools.Dbgen.CLI (CLI (..))
import           Pos.Tools.Dbgen.Lib (generateWalletDB, loadGenSpec)
import           Pos.Tools.Dbgen.Rendering (bold, say)
import           Pos.Tools.Dbgen.Stats (showStatsAndExit, showStatsData)
import           Pos.Tools.Dbgen.Types (UberMonad)

defaultNetworkConfig :: Topology kademlia -> NetworkConfig kademlia
defaultNetworkConfig ncTopology = NetworkConfig {
      ncDefaultPort   = 3000
    , ncSelfName      = Nothing
    , ncEnqueuePolicy = topologyEnqueuePolicy ncTopology
    , ncDequeuePolicy = topologyDequeuePolicy ncTopology
    , ncFailurePolicy = topologyFailurePolicy ncTopology
    , ncTcpAddr       = TCP.Unaddressable
    , ..
    }

newRealModeContext
    :: HasConfigurations
    => ProtocolMagic
    -> TxpConfiguration
    -> NodeDBs
    -> ConfigurationOptions
    -> FilePath
    -> FilePath
    -> IO (RealModeContext ())
newRealModeContext pm txpConfig dbs confOpts publicKeyPath secretKeyPath = do
    let nodeArgs = NodeArgs {
      behaviorConfigPath = Nothing
    }
    let networkOps = NetworkConfigOpts {
          ncoTopology = Nothing
        , ncoKademlia = Nothing
        , ncoSelf     = Nothing
        , ncoPort     = 3030
        , ncoPolicies = Nothing
        , ncoBindAddress = Nothing
        , ncoExternalAddress = Nothing
        }
    let cArgs@CommonNodeArgs {..} = CommonNodeArgs {
           dbPath                 = Just "node-db"
         , rebuildDB              = True
         , cnaAssetLockPath       = Nothing
         , devGenesisSecretI      = Nothing
         , publicKeyfilePath      = publicKeyPath
         , keyfilePath            = secretKeyPath
         , networkConfigOpts      = networkOps
         , jlPath                 = Nothing
         , commonArgs             = CommonArgs {
               logConfig            = Nothing
             , logPrefix            = Nothing
             , reportServers        = mempty
             , updateServers        = mempty
             , configurationOptions = confOpts
             }
         , updateLatestPath       = "update"
         , updateWithPackage      = False
         , route53Params          = Nothing
         , enableMetrics          = False
         , ekgParams              = Nothing
         , statsdParams           = Nothing
         , cnaDumpGenesisDataPath = Nothing
         , cnaDumpConfiguration   = False
         }
    loggerName <- askLoggerName
    nodeParams <- getNodeParams loggerName cArgs nodeArgs
    let vssSK = fromJust $ npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams cArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @() nodeParams gtParams (txpGlobalSettings pm txpConfig) (initNodeDBs pm epochSlots) $ \NodeResources{..} ->
        RealModeContext <$> pure dbs
                        <*> pure nrSscState
                        <*> pure nrTxpState
                        <*> pure nrDlgState
                        <*> jsonLogConfigFromHandle stdout
                        <*> pure (LoggerName "dbgen")
                        <*> pure nrContext
                        <*> pure noReporter
                        -- <*> initQueue (defaultNetworkConfig (TopologyAuxx mempty)) Nothing


walletRunner
    :: HasConfigurations
    => ProtocolMagic
    -> TxpConfiguration
    -> ConfigurationOptions
    -> NodeDBs
    -> FilePath
    -> FilePath
    -> WalletDB
    -> UberMonad a
    -> IO a
walletRunner pm txpConfig confOpts dbs publicKeyPath secretKeyPath ws act = do
    wwmc <- WalletWebModeContext <$> pure ws
                                 <*> newTVarIO def
                                 <*> liftIO newTQueueIO
                                 <*> newRealModeContext pm txpConfig dbs confOpts publicKeyPath secretKeyPath
    runReaderT act wwmc

newWalletState :: MonadIO m => Bool -> FilePath -> m WalletDB
newWalletState recreate walletPath =
    -- If the user passed the `--add-to` option, it means we don't have
    -- to rebuild the DB, but rather append stuff into it.
    liftIO $ openState (not recreate) walletPath

-- TODO(ks): Fix according to Pos.Client.CLI.Options
newConfig :: CLI -> ConfigurationOptions
newConfig CLI{..} = defaultConfigurationOptions {
      cfoSystemStart  = Timestamp . fromMicroseconds . fromIntegral . (*) 1000000 <$> systemStart
    , cfoFilePath     = configurationPath
    , cfoKey          = toText configurationProf
    }

-- stack exec dbgen -- --config ./tools/src/dbgen/config.json --nodeDB db-mainnet --walletDB wdb-mainnet --configPath node/configuration.yaml --secretKey secret-mainnet.key --configProf mainnet_full
main :: IO ()
main = do

    cli@CLI{..} <- getRecord "DBGen"
    let cfg = newConfig cli

    withConfigurations Nothing cfg $ \pm txpConfig _ -> do
        when showStats (showStatsAndExit walletPath)

        say $ bold "Starting the modification of the wallet..."

        showStatsData "before" walletPath

        dbs  <- openNodeDBs False nodePath -- Do not recreate!
        spec <- loadGenSpec config
        ws   <- newWalletState (isJust addTo) walletPath -- Recreate or not

        let generatedWallet = generateWalletDB cli spec
        walletRunner pm txpConfig cfg dbs publicKeyPath secretKeyPath ws generatedWallet
        closeState ws

        showStatsData "after" walletPath
