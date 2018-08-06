{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Demo
    (
    -- * Start a cluster of wallet nodes
      startCluster
    , waitForNode

    -- * WalletClient (run requests against the API)
    , WalletClient
    , mkWHttpClient

    -- * Bootstraping nodes
    , generateInitialState
    ) where

import           Universum hiding (takeWhile)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, async)
import           Data.Attoparsec.ByteString.Char8 (IResult (..), parse,
                     skipWhile, string, takeWhile)
import           Data.List (elemIndex, stripPrefix)
import           Data.Maybe (fromJust)
import           Data.X509.File (readSignedObject)
import           Options.Applicative (ParseError (ShowHelpText), Parser,
                     ParserHelp (..), ParserInfo, defaultPrefs, execFailure,
                     execParser, info, parserFailure)
import           Options.Applicative.Help.Chunk (Chunk (..))
import           System.Environment (getEnv, getEnvironment, lookupEnv, setEnv,
                     withArgs)

import           Cardano.Wallet.Client (ClientError (..),
                     ServantError (ConnectionError), WalletClient (..))
import           Cardano.Wallet.Client.Http (BaseUrl (..), Scheme (..),
                     credentialLoadX509, liftClient, mkHttpClient,
                     mkHttpsManagerSettings, newManager)
import           Cardano.Wallet.Launcher (runWWebMode, startWalletNode)
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     WalletBackendParams (..), WalletDBOptions (..),
                     WalletStartupOptions (..), walletBackendParamsParser)
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeArgsParser)
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Core.Configuration (generatedSecrets)
import           Pos.Core.Constants (accountGenesisIndex, wAddressGenesisIndex)
import           Pos.Core.Genesis (GeneratedSecrets (..), PoorSecret,
                     poorSecretToEncKey)
import           Pos.Core.NetworkAddress (NetworkAddress, addrParser)
import           Pos.Crypto.Signing (PassPhrase)
import           Pos.Launcher (HasConfigurations, LoggingParams (..))
import           Pos.Util.UserSecret (WalletUserSecret (..))
import           Pos.Wallet.Web.Methods (importWalletDo)
import           Pos.Wallet.Web.Mode (WalletWebMode)


import qualified Data.ByteString.Char8 as B8
import qualified Data.Char as Char
import qualified Data.List as List
import qualified Text.Parsec as Parsec


-- | Start a cluster of wallet nodes in different thread with the given NodeIds.
-- Node gets their argument from the ENVironment.
startCluster :: String -> [String] -> IO [Async ()]
startCluster prefix nodes = do
    let cVars = varFromParser commonNodeArgsParser prefix
    let nVars = varFromParser nodeArgsParser prefix
    let wVars = varFromParser walletBackendParamsParser prefix

    (prefix <> "REBUILD_DB")         `as` "True"
    (prefix <> "LISTEN")             `as` "127.0.0.1:3000"
    (prefix <> "TOPOLOGY")           `as` (toStateFolder prefix <> "/topology.yaml")
    (prefix <> "TLSCERT")            `as` (toStateFolder prefix <> "/tls/server.crt")
    (prefix <> "TLSKEY")             `as` (toStateFolder prefix <> "/tls/server.key")
    (prefix <> "TLSCA")              `as` (toStateFolder prefix <> "/tls/ca.crt")
    (prefix <> "WALLET_REBUILD_DB")  `as` "True"
    (prefix <> "WALLET_ADDRESS")     `as` "127.0.0.1:8090"
    (prefix <> "CONFIGURATION_FILE") `as` "../lib/configuration.yaml"
    (prefix <> "CONFIGURATION_KEY")  `as` "default"
    (prefix <> "SYSTEM_START")       `as` "0"
    (prefix <> "DB_PATH")            `as` (toStateFolder prefix <> "/db/")
    (prefix <> "WALLET_DB_PATH")     `as` (toStateFolder prefix <> "/wallet-db/")
    (prefix <> "LOG_CONFIG")         `as` (toStateFolder prefix <> "/logs/")

    -- NOTE
    -- Variables below are treated a bit differently because they're different
    -- for each node. We use the initial ENV var as a starting point and then
    -- adjust the variable's value in function of the current node, incrementing
    -- between each step.
    dbPath       <- getEnv       (prefix <> "DB_PATH")
    walletDbPath <- getEnv       (prefix <> "WALLET_DB_PATH")
    logConfig    <- getEnv       (prefix <> "LOG_CONFIG")
    addr         <- getNtwrkAddr (prefix <> "LISTEN")
    waddr        <- getNtwrkAddr (prefix <> "WALLET_ADDRESS")

    let indexedNodes = zip nodes (iterate (+1) 0)

    forM indexedNodes $ \(nodeId, i) -> do
        setEnv (prefix <> "NODE_ID")            nodeId
        setEnv (prefix <> "LISTEN")             (ntwrkAddrToEnv $ nextNtwrkAddr i addr)
        setEnv (prefix <> "WALLET_ADDRESS")     (ntwrkAddrToEnv $ nextNtwrkAddr i waddr)
        setEnv (prefix <> "WALLET_DOC_ADDRESS") (ntwrkAddrToEnv $ nextNtwrkAddr (i+100) waddr)
        setEnv (prefix <> "DB_PATH")            (dbPath <> nodeId)
        setEnv (prefix <> "WALLET_DB_PATH")     (walletDbPath <> nodeId)
        setEnv (prefix <> "LOG_CONFIG")         (logConfig <> nodeId <> ".yaml")

        cArgs <- execParserEnv cVars prefix (info commonNodeArgsParser mempty)
        nArgs <- execParserEnv nVars prefix (info nodeArgsParser mempty)
        wArgs <- execParserEnv wVars prefix (info walletBackendParamsParser mempty)

        -- NOTE
        -- We manually force two things here:
        --   - No logging in the console (the 'printOutput' parameter from the
        --     logger yaml config file is overwritten by code).
        --
        --   - We do not rebuild the wallet DB because we initialize it just
        --     before with 'generateInitialState'. Note that the db is cleared
        --     anyway when the demo is restarted.
        let lArgs = (loggingParams (fromString nodeId) cArgs) { lpConsoleLog = Just False }
        let wOpts = WalletStartupOptions cArgs (WalletLegacy $ wArgs
                { walletDbOptions = (walletDbOptions wArgs)
                    { walletRebuildDb = False
                    }
                })

        async $ do
            runWWebMode cArgs nArgs wArgs generateInitialState
            startWalletNode nArgs wOpts lArgs


-- | Create a Wallet Http Client using some ENVironment variables
mkWHttpClient :: MonadIO m => String -> IO (WalletClient m)
mkWHttpClient prefix = do
    tlsClientCertPath <- getEnvD (prefix <> "TLSCERT_CLIENT") (toStateFolder prefix <> "/tls/client.crt") getEnv
    tlsClientKeyPath  <- getEnvD (prefix <> "TLSKEY_CLIENT")  (toStateFolder prefix <> "/tls/client.key") getEnv
    tlsCACertPath     <- getEnvD (prefix <> "TLSCA")          (toStateFolder prefix <> "/tls/ca.crt")     getEnv
    (host, port)      <- getEnvD (prefix <> "WALLET_ADDRESS") "127.0.0.1:8090"                            getNtwrkAddr

    let serverId = (B8.unpack host, B8.pack $ show port)
    caChain <- readSignedObject tlsCACertPath
    clientCredentials <- credentialLoadX509Unsafe tlsClientCertPath tlsClientKeyPath
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials
    let baseUrl = BaseUrl Https (fst serverId) (fromIntegral port) mempty

    return $ liftClient $ mkHttpClient baseUrl manager
  where
    credentialLoadX509Unsafe path =
        let
            orFail =
                either (fail . ("Error decoding X509 certificates: " <>)) return
        in
            credentialLoadX509 path >=> orFail


-- | Make HttpRequest continuously to wait after the node
waitForNode :: WalletClient IO -> IO ()
waitForNode wc = do
    resp <- getNodeInfo wc
    case resp of
        Right _ ->
            return ()

        Left (ClientHttpError ConnectionError{}) ->
            threadDelay 1000000 >> waitForNode wc

        Left err ->
            fail (show err)


-- | Generate an initial state containing wallet with already some coins. Those
-- wallets come from the genesis state coming along with a TestnetInitializer.
generateInitialState :: HasConfigurations => WalletWebMode ()
generateInitialState = do
    wallets <- generatedSecretsToWalletSecrets <$> getGeneratedSecrets
    forM_ wallets (uncurry importWalletDo)
  where
    getGeneratedSecrets :: (MonadFail m) => m GeneratedSecrets
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
                , _wusWalletName = "Genesis Wallet (Poor)"
                , _wusAccounts   = [(accountGenesisIndex, "Genesis Account")]
                , _wusAddrs      = [(accountGenesisIndex, wAddressGenesisIndex)]
                }
        in
            (mempty, walUserSecret)


--
-- INTERNALS
--
--

data ArgType = Arg | Flag deriving Show

-- | Define a default ENV var if it doesn't exist
as :: String -> String -> IO ()
as var def =
    setEnv var =<< (fromMaybe def <$> lookupEnv var)

-- | get/set an ENV variable with a default, speficying the getter
getEnvD :: String -> String -> (String -> IO a) -> IO a
getEnvD var def getEnv' =
    var `as` def >> getEnv' var

-- | (unsafe) Parse a NetworkAddress from an ENV var
getNtwrkAddr :: String -> IO NetworkAddress
getNtwrkAddr =
    getEnv >=> (either (fail . show) return . Parsec.parse addrParser "" . toText)

-- | Get the next NetworkAddress given an index
nextNtwrkAddr :: Word16 -> NetworkAddress -> NetworkAddress
nextNtwrkAddr i (host, port) =
    (host, port + i)

-- | Convert a NetworkAddress to a string suitable for the Environment
ntwrkAddrToEnv :: NetworkAddress -> String
ntwrkAddrToEnv (host, port) =
    B8.unpack host <> ":" <> show port

-- | Extract the list of ENV var from a 'Options.Applicative.Parser'
varFromParser :: Parser a -> String -> [(String, ArgType)]
varFromParser parser p =
    let
        (usage, _, _) = execFailure (parserFailure defaultPrefs (info parser mempty) ShowHelpText mempty) ""

        usageS = B8.pack $ show $ fromJust $ unChunk $ helpUsage usage

        capture = skipWhile (/= '[') *> string "[" *> takeWhile (/= ']') <* string "]"

        addPrefix (s, t) = (p <> s, t)

        foldParse xs str = case parse (argToVar . B8.unpack <$> capture) str of
            Fail{}      -> xs
            Partial{}   -> xs
            Done rest x -> foldParse (x : xs) rest
    in
        map addPrefix (foldParse [] usageS)

-- | Replace third argument by the second one if it matches the first one.
replaceIf :: Char -> Char -> Char -> Char
replaceIf want to x | x == want = to
replaceIf _ _ x     = x

-- | Remove a prefix, throw if there's no such prefix
unsafeStripPrefix :: String -> String -> String
unsafeStripPrefix p =
    fromJust . stripPrefix p

-- | kebab-case to UPPER_SNAKE_CASE
kToS :: String -> String
kToS = map (Char.toUpper . replaceIf '-' '_')

-- | UPPER_SNAKE_CASE to kebab-case
sToK :: String -> String
sToK = map (Char.toLower . replaceIf '_' '-')

-- | Convert a prefix to a state folder name
toStateFolder :: String -> String
toStateFolder =
    ("state-" <>) . sToK . List.init

-- | Convert a string argument to its corresponding ENV var
argToVar :: String -> (String, ArgType)
argToVar arg = case elemIndex ' ' arg of
    Nothing -> (kToS (drop 2 arg), Flag)
    Just i  -> (kToS (drop 2 (take i arg)), Arg)

-- | Convert an environment variable to its argument, with value. Returns
-- 'Nothing' when Flags are given and turned off. 'Just arg' otherwise.
varToArg :: (String, ArgType, String) -> Maybe String
varToArg = \case
    (key, Flag, "True") -> Just ("--" <> sToK key)
    (_, Flag, _)        -> Nothing
    (key, Arg, val)     -> Just ("--" <> sToK key <> "=" <> val)

-- | Run a parser from environment variables rather than command-line arguments
execParserEnv
    :: [(String, ArgType)] -- ^ The restricted environment
    -> String              -- ^ A prefix to remove from all Env variables
    -> ParserInfo a        -- ^ A corresponding CLI
    -> IO a
execParserEnv vars p pInfo = do
    args <- (mapMaybe varToArg . mapMaybe filterEnv) <$> getEnvironment
    withArgs args $ execParser pInfo
  where
    filterEnv :: (String, String) -> Maybe (String, ArgType, String)
    filterEnv (k, v) =
        (\(_, t) -> (unsafeStripPrefix p k, t, v)) <$> find ((== k) . fst) vars
