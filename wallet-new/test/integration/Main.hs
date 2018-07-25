module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Default (Default (..))
import           Data.Time.Units (fromMicroseconds)
import           System.Environment (lookupEnv)
import           Test.Hspec (describe, hspec, it, shouldBe)

import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
-- import           Pos.Client.CLI.Options (CommonArgs (..))
import           Pos.Client.CLI.Params (getNodeParams, gtSscParams)
import           Pos.Core (ProtocolMagic, Timestamp (..))
import           Pos.Core.Configuration (defaultGenesisSpec, epochSlots,
                     withProtocolConstants)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedGenesisData (..),
                     GeneratedSecrets (..), GenesisProtocolConstants (..),
                     GenesisSpec (..), PoorSecret, generateGenesisData,
                     genesisProtocolConstantsToProtocolConstants,
                     poorSecretToEncKey)
import           Pos.Core.JsonLog.LogEvents (jsonLogConfigFromHandle)
import           Pos.Core.Reporting (noReporter)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.DB (NodeDBs, closeNodeDBs, openNodeDBs)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), NodeResources (..), bracketNodeResources,
                     withConfigurations)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.UserSecret (WalletUserSecret (..), usVss)
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode, WalletWebModeContext (..))
import           Pos.Wallet.Web.State.Acidic (closeState, openState)
import           Pos.WorkMode (RealModeContext (..))
import           System.Wlog (HasLoggerName (askLoggerName))


--      INTEGRATION_TESTS_NODE_PATH   = Path to a valid rocksdb database
--      INTEGRATION_TESTS_WALLET_PATH = Path to a valid acid-state database
--      INTEGRATION_TESTS_DB_PATH     = Path to directory with all DBs used by the node
--      INTEGRATION_TESTS_CONFIG_PATH = Path to the yaml configuration file
--      INTEGRATION_TESTS_CONFIG_KEY  = Key to use within that config file (e.g.  --      development, test ...)
main :: IO ()
main = do
    nodePath    <- lookupEnvD "../integration_tests/node" "INTEGRATION_TESTS_NODE_PATH"
    walletPath  <- lookupEnvD "../integration_tests/wallet" "INTEGRATION_TESTS_WALLET_PATH"
    dbPath      <- lookupEnvD "../integration_tests/db" "INTEGRATION_TESTS_DB_PATH"
    configPath  <- lookupEnvD "../lib/configuration.yaml" "INTEGRATION_TESTS_CONFIG_PATH"
    configKey   <- lookupEnvD "test" "INTEGRATION_TESTS_CONFIG_KEY"

    let args =
            ( def
                { dbPath    = Just dbPath
                , rebuildDB = True
                }
            , def
            , ExtraNodeArgs
                { _nodePath   = nodePath
                , _walletPath = walletPath
                , _configPath = configPath
                , _configKey  = toText configKey
                }
            )

    generateInitialState args defaultGenesisSpec
    hspec $ describe "Integration tests" $ do
        it "A first test" $
            True `shouldBe` True

        it "A second test" $
            False `shouldBe` False
  where
    --lookupEnvE :: String -> IO String
    --lookupEnvE var = do
    --    let msg = "Missing required ENV var '" <> var <> "'"
    --    maybe (fail msg) return =<< lookupEnv var

    lookupEnvD :: String -> String -> IO String
    lookupEnvD d var =
        fromMaybe d <$> lookupEnv var


data ExtraNodeArgs = ExtraNodeArgs
    { _nodePath   :: !FilePath
    , _walletPath :: !FilePath
    , _configPath :: !FilePath
    , _configKey  :: !Text
    }


-- | Generate an initial state for the integration tests
--
-- Example:
--    import Pos.Core.Configuration(defaultGenesisSpec)
--
--    generateInitialState defaultGenesisSpec
generateInitialState
    :: (CommonNodeArgs, NodeArgs, ExtraNodeArgs)
    -> GenesisSpec
    -> IO ()
generateInitialState args spec = do
    let wallets = genesisDataToWalletUserSecrets . genesisSpecToGenesisData $ spec
    runWalletWebMode args $ forM_ wallets (uncurry importWalletDo)


-- | Run a particular wallet action in a default context.
--
-- Note that a few things are configurable through environment variable:
--
runWalletWebMode
    :: (CommonNodeArgs, NodeArgs, ExtraNodeArgs)
    -> (HasConfigurations => WalletWebMode a)
    -> IO a
runWalletWebMode (commonNodeArgs, nodeArgs, ExtraNodeArgs{..}) action = do
    let configOpts = ConfigurationOptions
            { cfoFilePath    = _configPath
            , cfoKey         = _configKey
            , cfoSystemStart = Just (Timestamp (fromMicroseconds 1512847931))
            , cfoSeed        = Nothing
            }

    withConfigurations Nothing configOpts
        $ \_ protocolMagic -> bracket (openState True _walletPath) closeState
        $ \walletState -> bracket (openNodeDBs True _nodePath) closeNodeDBs
        $ \nodeDBs -> do
            ctx <- WalletWebModeContext
                <$> pure walletState
                <*> newTVarIO def
                <*> liftIO newTQueueIO
                <*> newRealModeContext protocolMagic nodeDBs
            action `runReaderT` ctx
  where
    -- | Create a new Real Mode context needed to run wallet's actions
    newRealModeContext
        :: HasConfigurations
        => ProtocolMagic
        -> NodeDBs
        -> IO (RealModeContext ())
    newRealModeContext pm dbs = do
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
