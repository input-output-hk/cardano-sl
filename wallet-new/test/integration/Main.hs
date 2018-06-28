module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Default (Default (..))
import           Data.Maybe (fromJust)
import           System.Environment (lookupEnv)

import           Mockable (Production (..))
import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..))
import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Params (getNodeParams, gtSscParams)
import           Pos.Core (ProtocolMagic)
import           Pos.Core.Configuration (defaultGenesisSpec, epochSlots,
                     withGeneratedSecrets, withProtocolConstants)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedGenesisData (..),
                     GeneratedSecrets (..), GenesisInitializer (..),
                     GenesisProtocolConstants (..), GenesisSpec (..),
                     PoorSecret, generateGenesisData,
                     genesisProtocolConstantsToProtocolConstants,
                     poorSecretToEncKey)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.DB (NodeDBs, closeNodeDBs, openNodeDBs)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), NodeResources (..), bracketNodeResources,
                     withConfigurations)
import           Pos.Sinbin.Reporting.Methods (noReporter)
import           Pos.Sinbin.Util.JsonLog.Events (jsonLogConfigFromHandle)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.UserSecret (WalletUserSecret (..), usVss)
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.WorkMode (RealModeContext (..))
import           System.Wlog (HasLoggerName (..), LoggerName (..))


instance HasLoggerName IO where
    askLoggerName =
        pure (LoggerName "test-integration")

    modifyLoggerName _ x =
        x

-- | Create a new Real Mode context needed to run wallet's actions
newRealModeContext
    :: HasConfigurations
    => ProtocolMagic
    -> NodeDBs
    -> ConfigurationOptions
    -> FilePath
    -> Production (RealModeContext ())
newRealModeContext pm dbs confOpts dbPath = do
    let commonNodeArgs = def
            { dbPath     = Just dbPath
            , commonArgs = def { configurationOptions = confOpts }
            }
    let nodeArgs = def
    loggerName <- askLoggerName
    nodeParams <- getNodeParams loggerName commonNodeArgs nodeArgs
    let (Just vssSK) = npUserSecret nodeParams ^. usVss
    let gtParams = gtSscParams commonNodeArgs vssSK (npBehaviorConfig nodeParams)
    bracketNodeResources @() nodeParams gtParams (txpGlobalSettings pm) (initNodeDBs pm epochSlots)
        $ \NodeResources{..} -> RealModeContext
            <$> pure dbs
            <*> pure nrSscState
            <*> pure nrTxpState
            <*> pure nrDlgState
            <*> jsonLogConfigFromHandle stdout
            <*> pure loggerName
            <*> pure nrContext
            <*> pure noReporter


-- | Run a particular wallet action in a default context.
--
-- Note that a few things are configurable through environment variable:
--
--      TEST_INTEGRATION_NODE_PATH   = Path to a valid rocksdb database
--      TEST_INTEGRATION_WALLET_PATH = Path to a valid acid-state database
--      TEST_INTEGRATIoN_DB_PATH     = Path to directory with all DBs used by the node
runWalletWebMode :: (HasConfigurations => WalletWebMode a) -> IO a
runWalletWebMode action = do
    nodePath <- getEnvF envPrefix "NODE_PATH"
    walletPath <- getEnvF envPrefix "WALLET_PATH"
    dbPath <- getEnvF envPrefix "DB_PATH"

    withConfigurations Nothing def
        $ \_ protocolMagic -> bracket (openState True walletPath) closeState
        $ \walletState -> bracket (openNodeDBs False nodePath) closeNodeDBs
        $ \nodeDBs -> runProduction $ do
            ctx <- WalletWebModeContext
                <$> pure walletState
                <*> newTVarIO def
                <*> liftIO newTQueueIO
                <*> newRealModeContext protocolMagic nodeDBs def dbPath
            action `runReaderT` ctx
  where
    envPrefix :: String
    envPrefix =
        "TEST_INTEGRATION"

    getEnvF :: String -> String -> IO a
    getEnvF prefix var = do
        let fullName = prefix <> "_" <> var
        let msg = "Missing required ENV var '" <> fullName <>"'"
        maybe (fail msg) return =<< lookupEnv fullName


genesisSpecToGenesisData :: GenesisSpec -> GeneratedGenesisData
genesisSpecToGenesisData spec =
    let
        gsCst             = gsProtocolConstants spec
        protocolConstants = genesisProtocolConstantsToProtocolConstants gsCst
        protocolMagic     = gpcProtocolMagic gsCst
        initializer       = gsInitializer spec
        balances          = gsAvvmDistr spec
    in
        withProtocolConstants protocolConstants $
            generateGenesisData protocolMagic initializer balances


genesisDataToWalletUserSecrets :: GeneratedGenesisData -> [(PassPhrase, WalletUserSecret)]
genesisDataToWalletUserSecrets genData =
    map poorSecretToWalletUserSecrets (gsPoorSecrets . ggdSecrets $ genData)
  where
    poorSecretToWalletUserSecrets :: PoorSecret -> (PassPhrase, WalletUserSecret)
    poorSecretToWalletUserSecrets secret =
        let
            walUserSecret = WalletUserSecret
                { _wusRootKey    = poorSecretToEncKey secret
                , _wusWalletName = "Genesis Test Wallet (Poor)"
                , _wusAccounts   = [(accountGenesisIndex, "Genesis Test Account")]
                , _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
                }
        in
            (mempty, walUserSecret)


-- | Generate an initial state for the integration tests
--
-- Example:
--    import Pos.Core.Configuration(defaultGenesisSpec)
--
--    generateInitialState defaultGenesisSpec
generateInitialState :: GenesisSpec -> IO ()
generateInitialState spec = do
    let wallets = genesisDataToWalletUserSecrets . genesisSpecToGenesisData $ spec
    runWalletWebMode $ forM_ wallets (uncurry importWalletDo)


main :: IO ()
main =
    generateInitialState defaultGenesisSpec
