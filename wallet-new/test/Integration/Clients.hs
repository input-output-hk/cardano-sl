module Integration.Clients
    (
    -- * Start a cluster of wallet nodes
    startCluster

    -- * WalletClient (run requests against the API)
    , WalletClient
    , mkWHttpClient
    ) where

import           Universum

import           Control.Concurrent (ThreadId, forkIO)
import           Data.X509.File (readSignedObject)
import           Options.Applicative (info)
import           System.Environment (getEnv, lookupEnv, setEnv, unsetEnv)
import           System.Wlog (LoggerName (..))

import           Cardano.Wallet.Client.Http (BaseUrl (..), Scheme (..),
                     WalletClient, credentialLoadX509, liftClient,
                     mkHttpClient, mkHttpsManagerSettings, newManager)
import           Cardano.Wallet.Launcher (runWWebMode, startWalletNode)
import           Cardano.Wallet.Server.CLI (ChooseWalletBackend (..),
                     WalletStartupOptions (..), execParserEnv,
                     walletBackendParamsParser)
import           Integration.Fixtures (generateInitialState)
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeArgsParser)
import           Pos.Core.NetworkAddress (NetworkAddress, addrParser)

import qualified Data.ByteString.Char8 as B8
import qualified Text.Parsec as Parsec


prefix :: String
prefix =
    "INTEGRATION_TESTS_"

blacklist :: [String]
blacklist =
    [ "INTEGRATION_TESTS_TLSCERT_CLIENT"
    , "INTEGRATION_TESTS_TLSKEY_CLIENT"
    ]


startCluster :: [String] -> IO [ThreadId]
startCluster nodes = do
    (prefix <> "REBUILD_DB")        `as` "True"
    (prefix <> "LISTEN")            `as` "127.0.0.1:3000"
    (prefix <> "TOPOLOGY")          `as` "state-integration-tests/topology.yaml"
    (prefix <> "TLSCERT")           `as` "state-integration-tests/tls/server.crt"
    (prefix <> "TLSKEY")            `as` "state-integration-tests/tls/server.key"
    (prefix <> "TLSCA")             `as` "state-integration-tests/tls/ca.crt"
    (prefix <> "WALLET_REBUILD_DB") `as` "True"
    (prefix <> "WALLET_ADDRESS")    `as` "127.0.0.1:8090"

    let indexedNodes = zip nodes (iterate (+1) 0)

    withoutEnv blacklist $ forM indexedNodes $ \(nodeId, i) -> do
        let lName  = LoggerName (toText nodeId)

        addr  <- nextNtwrkAddr i <$> getNtwrkAddr (prefix <> "LISTEN")
        waddr <- nextNtwrkAddr i <$> getNtwrkAddr (prefix <> "WALLET_ADDRESS")

        (prefix <> "NODE_ID")        `as` nodeId
        (prefix <> "DB_PATH")        `as` ("state-integration-tests/db/" <> nodeId)
        (prefix <> "WALLET_DB_PATH") `as` ("state-integration-tests/wallet-db/" <> nodeId)
        (prefix <> "LOG_CONFIG")     `as` ("state-integration-tests/logs/" <> nodeId <> ".yaml")
        (prefix <> "LISTEN")         `as` ntwrkAddrToEnv addr
        (prefix <> "WALLET_ADDRESS") `as` ntwrkAddrToEnv waddr

        cArgs <- execParserEnv prefix (info commonNodeArgsParser mempty)
        nArgs <- execParserEnv prefix (info nodeArgsParser mempty)
        wArgs <- execParserEnv prefix (info walletBackendParamsParser mempty)

        let wOpts = WalletStartupOptions cArgs (WalletLegacy wArgs)

        runWWebMode cArgs nArgs wArgs lName generateInitialState

        forkIO $ startWalletNode nArgs wOpts lName


mkWHttpClient :: MonadIO m => IO (WalletClient m)
mkWHttpClient = do
    tlsClientCertPath <- getEnvD (prefix <> "TLSCERT_CLIENT") "state-integration-tests/tls/client.crt"  getEnv
    tlsClientKeyPath  <- getEnvD (prefix <> "TLSKEY_CLIENT")  "state-integration-tests/tls/client.key"  getEnv
    tlsCACertPath     <- getEnvD (prefix <> "TLSCA")          "state-integration-tests/tls/ca.crt"      getEnv
    (host, port)      <- getEnvD (prefix <> "WALLET_ADDRESS") "127.0.0.1:8090"                          getNtwrkAddr

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


--
-- INTERNALS
--

-- | Define a default ENV var if it doesn't exist
as :: String -> String -> IO ()
as var def =
    setEnv var =<< (fromMaybe def <$> lookupEnv var)

-- | Run an IO action excluding some specific environment variable
withoutEnv :: [String] -> IO a -> IO a
withoutEnv vars io  =
    let
        backupVar k = do
            v <- lookupEnv k
            unsetEnv k
            return v

        restoreVar _ Nothing  = return ()
        restoreVar k (Just v) =  setEnv k v
    in
        bracket (mapM backupVar vars) (zipWithM restoreVar vars) (const io)

-- | get/set an ENV variable with a default, speficying the getter
getEnvD :: String -> String -> (String -> IO a) -> IO a
getEnvD var def getEnv' =
    var `as` def >> getEnv' var

-- | (unsafe) Parse a NetworkAddress from an ENV var
getNtwrkAddr :: String -> IO NetworkAddress
getNtwrkAddr =
    either (fail . show) return . Parsec.parse addrParser "" .  toText

-- | Get the next NetworkAddress given an index
nextNtwrkAddr :: Word16 -> NetworkAddress -> NetworkAddress
nextNtwrkAddr i (host, port) =
    (host, port + i)

-- | Convert a NetworkAddress to a string suitable for the Environment
ntwrkAddrToEnv :: NetworkAddress -> String
ntwrkAddrToEnv (host, port) =
    B8.unpack host <> ":" <> show port
