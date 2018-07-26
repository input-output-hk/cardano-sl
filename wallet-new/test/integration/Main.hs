module Main where

import           Universum

import           Control.Concurrent.STM (newTQueueIO)
import           Data.Default (Default (..))
import           Data.Time.Units (fromMicroseconds)
import           System.Environment (lookupEnv)
import           Test.Hspec (describe, hspec, it, shouldBe)

import           Pos.Client.CLI.NodeOptions (CommonNodeArgs (..), NodeArgs (..))
import           Pos.Client.CLI.Params (getNodeParams, gtSscParams)
import           Pos.Core (Timestamp (..))
import           Pos.Core.Configuration (HasGeneratedSecrets, epochSlots,
                     generatedSecrets)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedSecrets (..), PoorSecret,
                     poorSecretToEncKey)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.DB.DB (initNodeDBs)
import           Pos.Infra.Diffusion.Types (Diffusion)
import           Pos.Launcher (ConfigurationOptions (..), HasConfigurations,
                     NodeParams (..), allocateNodeResources,
                     releaseNodeResources, withConfigurations)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Util.CompileInfo (HasCompileInfo, withCompileInfo)
import           Pos.Util.UserSecret (WalletUserSecret (..), usVss)
import           Pos.Wallet.WalletMode (WalletMempoolExt)
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Server.Runner (runWRealMode)
import           Pos.Wallet.Web.Sockets (closeWSConnections, initWSConnections)
import           Pos.Wallet.Web.State.Acidic (closeState, openState)


--      INTEGRATION_TESTS_NODE_PATH   = Path to a valid rocksdb database
--      INTEGRATION_TESTS_WALLET_PATH = Path to a valid acid-state database
--      INTEGRATION_TESTS_DB_PATH     = Path to directory with all DBs used by the node
--      INTEGRATION_TESTS_CONFIG_PATH = Path to the yaml configuration file
--      INTEGRATION_TESTS_CONFIG_KEY  = Key to use within that config file (e.g.  --      development, test ...)
main :: IO ()
main = do
    nodePath    <- lookupEnvD "state-integration-tests/node"    "INTEGRATION_TESTS_NODE_PATH"
    walletPath  <- lookupEnvD "state-integration-tests/wallet"  "INTEGRATION_TESTS_WALLET_PATH"
    dbPath      <- lookupEnvD "state-integration-tests/node-db" "INTEGRATION_TESTS_DB_PATH"
    configPath  <- lookupEnvD "../lib/configuration.yaml"       "INTEGRATION_TESTS_CONFIG_PATH"
    configKey   <- lookupEnvD "default"                            "INTEGRATION_TESTS_CONFIG_KEY"

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

    generateInitialState args

    hspec $ describe "Integration tests" $ do
        it "A first test" $
            True `shouldBe` True

        it "A second test" $
            False `shouldBe` False
  where
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
generateInitialState
    :: (CommonNodeArgs, NodeArgs, ExtraNodeArgs)
    -> IO ()
generateInitialState args = runWalletWebMode args $ \_ -> do
    wallets <- generatedSecretsToWalletSecrets <$> getGeneratedSecrets
    forM_ wallets (uncurry importWalletDo)
  where
    getGeneratedSecrets :: (MonadFail m, HasGeneratedSecrets) => m GeneratedSecrets
    getGeneratedSecrets = do
        let msg = "Couldn't find GeneratedSecrets. To fix this, make sure you \
                  \run the following program with a `TestnetInitializer`."
        maybe (fail msg) return generatedSecrets

    generatedSecretsToWalletSecrets :: GeneratedSecrets -> [(PassPhrase, WalletUserSecret)]
    generatedSecretsToWalletSecrets secrets =
        map poorSecretToWalletUserSecrets (gsPoorSecrets secrets)

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


runWalletWebMode
    :: forall a
    .  (CommonNodeArgs, NodeArgs, ExtraNodeArgs)
    -> ((HasConfigurations, HasCompileInfo) => Diffusion WalletWebMode -> WalletWebMode a)
    -> IO a
runWalletWebMode (commonNodeArgs, nodeArgs, ExtraNodeArgs{..}) action = do
    let configOpts = ConfigurationOptions
            { cfoFilePath    = _configPath
            , cfoKey         = _configKey
            , cfoSystemStart = Just (Timestamp (fromMicroseconds 1512847931))
            , cfoSeed        = Nothing
            }

    withCompileInfo $ withConfigurations Nothing configOpts
        $ \_ pm -> bracket (openState True _walletPath) closeState
        $ \db   -> bracket initWSConnections closeWSConnections
        $ \conn -> do
            params <- getNodeParams "integration-tests" commonNodeArgs nodeArgs
            let (Just vssSK) = npUserSecret params ^. usVss
            let ssc          = gtSscParams commonNodeArgs vssSK (npBehaviorConfig params)
            let txp          = txpGlobalSettings pm
            let mode         = initNodeDBs pm epochSlots
            let acquire      = allocateNodeResources @WalletMempoolExt params ssc txp mode
            bracket acquire releaseNodeResources $ \res -> do
                queue <- liftIO newTQueueIO
                runWRealMode pm db conn queue res action
