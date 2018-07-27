module Clients
    ( mkWWebModeRunner
    , WWebModeRunner(..)
    ) where

import           Universum

import           Data.Default (Default (..))
import           Data.Time.Units (fromMicroseconds)
import           System.Environment (lookupEnv)
import           System.Wlog (LoggerName (..))

import           Pos.Core (Timestamp (..))
import           Pos.Launcher (HasConfigurations)
import           Pos.Util.CompileInfo (HasCompileInfo)
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.Server.Runner (CommonNodeArgs (..),
                     ExtraNodeArgs (..))

import qualified Pos.Wallet.Web.Server.Runner as Runner
import qualified Prelude


-- NOTE This is needed because "GHC doesn't yet support impredicative polymorphism"
-- Therefore, we can't simply return the inner function from an IO, hence this
-- trick using a newtype.
newtype WWebModeRunner = WWebModeRunner
    { runWWebMode :: forall a. ((HasConfigurations, HasCompileInfo) => WalletWebMode a) -> IO a
    }


-- | Read environment variables and turn them into node arguments, using default
-- value for the rest. Then, return a runner able to execute (WalletWebMode a)
-- actions in IO using the provided config.
--
-- We can add more environment variables as needed if we need to tweak something.
mkWWebModeRunner :: IO WWebModeRunner
mkWWebModeRunner = do
    nodePath    <- lookupEnvD "state-integration-tests/node"    "INTEGRATION_TESTS_NODE_PATH"
    walletPath  <- lookupEnvD "state-integration-tests/wallet"  "INTEGRATION_TESTS_WALLET_PATH"
    dbPath      <- lookupEnvD "state-integration-tests/db"      "INTEGRATION_TESTS_DB_PATH"
    configPath  <- lookupEnvD "../lib/configuration.yaml"       "INTEGRATION_TESTS_CONFIG_PATH"
    configKey   <- lookupEnvD "default"                         "INTEGRATION_TESTS_CONFIG_KEY"
    systemStart <- lookupEnvD "1512847931"                      "INTEGRATION_TESTS_SYSTEM_START"

    let (commonNodeArgs, nodeArgs, extraNodeArgs) =
            ( def
                { dbPath    = Just dbPath
                , rebuildDB = True
                }
            , def
            , ExtraNodeArgs
                { _nodePath    = nodePath
                , _walletPath  = walletPath
                , _configPath  = configPath
                , _configKey   = toText configKey
                , _systemStart = Timestamp (fromMicroseconds (Prelude.read systemStart))
                , _loggerName  = LoggerName "integration-tests"
                }
            )

    return $ WWebModeRunner (Runner.runWWebMode commonNodeArgs nodeArgs extraNodeArgs)


--
-- INTERNALS
--

newtype DefaultValue
    = DefaultValue String
    deriving (Show)

instance IsString DefaultValue where
    fromString = DefaultValue

-- | Lookup environment variable with default
lookupEnvD :: DefaultValue -> String -> IO String
lookupEnvD (DefaultValue d) var =
    fromMaybe d <$> lookupEnv var
