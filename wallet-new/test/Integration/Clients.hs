{-# LANGUAGE QuasiQuotes #-}

module Integration.Clients
    ( describeOptions

    -- * WWebModeRunner (run WalletWebMode actions)
    , WWebModeRunner(..)
    , mkWWebModeRunner

    -- * WalletClient (run requests against the API)
    , WalletClient
    , mkWHttpClient
    ) where

import           Universum

import           Data.Default (Default (..))
import           Data.Time.Units (fromMicroseconds)
import           Data.X509.File (readSignedObject)
import           System.Environment (lookupEnv)
import           System.Wlog (LoggerName (..))

import           Cardano.Wallet.Client.Http (BaseUrl (..), Scheme (..),
                     WalletClient, credentialLoadX509, liftClient,
                     mkHttpClient, mkHttpsManagerSettings, newManager)
import           NeatInterpolation (text)
import           Pos.Core (Timestamp (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Server.Runner (CommonNodeArgs (..),
                     ExtraNodeArgs (..))

import qualified Data.ByteString.Char8 as B8
import qualified Pos.Wallet.Web.Server.Runner as Runner
import qualified Prelude


-- NOTE This is needed because "GHC doesn't yet support impredicative polymorphism"
-- Therefore, we can't simply return the inner function from an IO, hence this
-- trick using a newtype.
newtype (WWebModeRunner m) = WWebModeRunner
    { runWWebMode :: forall a. Monad m => ((HasConfigurations, HasCompileInfo) => WalletWebMode a) -> m a
    }


instance DescribeOptions WWebModeRunner where
    describeOptions _ = [text|
    INTEGRATION_TESTS_GENESIS_SECRET   = Override genesis secret from the config file (e.g. 14)
    INTEGRATION_TESTS_WALLET_PATH      = Path to the acid-state database (e.g. 'wallet-db')
    INTEGRATION_TESTS_DB_PATH          = Path to the rocksdb database (e.g. 'node-db')
    INTEGRATION_TESTS_CONFIG_PATH      = Path to the configuration file (e.g. 'lib/configuration.yaml')
    INTEGRATION_TESTS_CONFIG_KEY       = Configuration key to use (e.g. 'default')
    INTEGRATION_TESTS_SYSTEM_START     = Timestamp (is μs) at which the system has started (e.g. 1533025621472000)
|]

-- | Read environment variables and turn them into node arguments, using default
-- value for the rest. Then, return a runner able to execute (WalletWebMode a)
-- actions in IO using the provided config.
--
-- We can add more environment variables as needed if we need to tweak something.
mkWWebModeRunner :: IO (WWebModeRunner IO)
mkWWebModeRunner = do
    -- NOTE The following defaults have been selected based on the `demo-with-wallet-api.sh` script
    -- This way, we act directly as if we were one of cluster's node.
    genesisSecret <- lookupEnvD "3"                          "INTEGRATION_TESTS_GENESIS_SECRET"
    walletPath    <- lookupEnvD "../wallet-db"               "INTEGRATION_TESTS_WALLET_PATH"
    dbPath        <- lookupEnvD "../run/node-db3"            "INTEGRATION_TESTS_DB_PATH"
    configPath    <- lookupEnvD "../lib/configuration.yaml"  "INTEGRATION_TESTS_CONFIG_PATH"
    configKey     <- lookupEnvD "default"                    "INTEGRATION_TESTS_CONFIG_KEY"
    systemStart   <- lookupEnvD "0"                          "INTEGRATION_TESTS_SYSTEM_START"

    let (commonNodeArgs, nodeArgs, extraNodeArgs) =
            ( def
                { dbPath            = Just dbPath
                , rebuildDB         = True
                , devGenesisSecretI = Just (Prelude.read genesisSecret)
                }
            , def
            , ExtraNodeArgs
                { _walletPath  = walletPath
                , _configPath  = configPath
                , _configKey   = toText configKey
                , _systemStart = Timestamp (fromMicroseconds (Prelude.read systemStart))
                , _loggerName  = LoggerName "integration-tests"
                }
            )

    return $ WWebModeRunner (Runner.runWWebMode commonNodeArgs nodeArgs extraNodeArgs)


instance DescribeOptions WalletClient where
    describeOptions _ = [text|
    INTEGRATION_TESTS_CLIENT_CERT_PATH = Path to a x509 client certificate (e.g. 'tls-files/client.crt')
    INTEGRATION_TESTS_CLIENT_KEY_PATH  = Path to a corresponding client private key (e.g. 'tls-files/client.key')
    INTEGRATION_TESTS_CA_CERT_PATH     = Path to a x509 CA certificate (e.g. 'tls-files/ca.crt')
    INTEGRATION_TESTS_SERVER_HOST      = Wallet backend's host (e.g. 'localhost')
    INTEGRATION_TESTS_SERVER_PORT      = Wallet backend's port (e.g. 8090)
|]

mkWHttpClient :: MonadIO m => IO (WalletClient m)
mkWHttpClient = do
    tlsClientCertPath <- lookupEnvD "../run/tls-files/client.crt" "INTEGRATION_TESTS_CLIENT_CERT_PATH"
    tlsClientKeyPath  <- lookupEnvD "../run/tls-files/client.key" "INTEGRATION_TESTS_CLIENT_KEY_PATH"
    tlsCACertPath     <- lookupEnvD "../run/tls-files/ca.crt"     "INTEGRATION_TESTS_CA_CERT_PATH"
    serverHost        <- lookupEnvD "localhost"                   "INTEGRATION_TESTS_SERVER_HOST"
    serverPort        <- lookupEnvD "8090"                        "INTEGRATION_TESTS_SERVER_PORT"

    let serverId = (serverHost, B8.pack serverPort)
    caChain <- readSignedObject tlsCACertPath
    clientCredentials <- credentialLoadX509Unsafe tlsClientCertPath tlsClientKeyPath
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials
    let baseUrl = BaseUrl Https serverHost (Prelude.read serverPort) mempty

    return $ liftClient $ mkHttpClient baseUrl manager
  where
    credentialLoadX509Unsafe path =
        let
            orFail =
                either (fail . ("Error decoding X509 certificates: " <>)) return
        in
            credentialLoadX509 path >=> orFail


--
-- INTERNALS
--

class DescribeOptions (a :: (* -> *) -> *) where
    describeOptions :: Proxy a -> Text

newtype DefaultValue
    = DefaultValue String
    deriving (Show)

instance IsString DefaultValue where
    fromString = DefaultValue

-- | Lookup environment variable with default
lookupEnvD :: DefaultValue -> String -> IO String
lookupEnvD (DefaultValue d) var =
    fromMaybe d <$> lookupEnv var
