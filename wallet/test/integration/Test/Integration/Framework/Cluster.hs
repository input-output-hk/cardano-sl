module Test.Integration.Framework.Cluster
    ( startCluster
    , waitForNode
    ) where

import           Universum hiding (init)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (race)
import           Data.Map.Strict ((!))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Options.Applicative (handleParseResult, info)
import qualified Prelude
import           System.Environment (getEnvironment)
import           System.FilePath ((</>))

import           Cardano.Cluster (MaxWaitingTime (..), NodeName (..),
                     NodeType (..), startNode)
import           Cardano.Cluster.Environment (Artifact (..), Env,
                     prepareEnvironment, withSystemStart)
import           Cardano.Cluster.Util (execParserEnv, oneSecond, runAsync,
                     stripFilterPrefix, varFromParser)
import           Cardano.Wallet.Action (actionWithWallet)
import           Cardano.Wallet.Client.Http (ClientError (..), Manager,
                     ServantError (..), WalletClient (getNodeInfo),
                     WalletHttpClient)
import           Cardano.Wallet.Server.CLI (walletBackendParamsParser)
import           Pos.Chain.Genesis (GeneratedSecrets (..),
                     configGeneratedSecretsThrow)
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeArgsParser)
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Launcher (LoggingParams (..), launchNode)
import           Pos.Launcher.Configuration (ConfigurationOptions (..),
                     withConfigurations)
import           Pos.Node.API (ForceNtpCheck (..))
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Wlog.Compatibility (usingNamedPureLogger)


prefix :: String
prefix = "INTEGRATION_"

-- | All those can be overriden by environment variables. These values
-- correspond to command line arguments that would be passed to underlying
-- processes.
--
-- As an example, if you wanted to enable the @--wallet-debug@ option for the
-- underlying node, you would add an entry in this list:
--
-- > ("WALLET_DEBUG", "True")
--
-- Underscores (@_@) are converted to hyphens (@-@), the text is lowercased, and
-- a leading @--@ is added.
defaultIntegrationEnv :: Env
defaultIntegrationEnv = Map.fromList
    [ ("CONFIGURATION_FILE", "./test/integration/configuration.yaml")
    , ("CONFIGURATION_KEY", "default")
    , ("STATE_DIR", "./state-integration")
    , ("REBUILD_DB", "True")
    , ("WALLET_ADDRESS", "127.0.0.1:8090")
    , ("WALLET_DOC_ADDRESS", "127.0.0.1:8190")
    , ("WALLET_DB_PATH", "./state-integration/wallet-db/edge")
    , ("WALLET_REBUILD_DB", "True")
    , ("WALLET_NODE_API_ADDRESS", "127.0.0.1:8089")
    , ("NODE_API_ADDRESS", "127.0.0.1:8086")
    , ("NODE_DOC_ADDRESS", "127.0.0.1:3186")
    , ("NODE_TLS_CLIENT_CERT", "./state-integration/tls/relay/client.crt")
    , ("NODE_TLS_KEY", "./state-integration/tls/relay/client.key")
    , ("NODE_TLS_CA_CERT", "./state-integration/tls/relay/ca.crt")
    ]

-- | Start an integration cluster. Quite identical to the original "start cluster".
-- The main difference here is that we start a wallet node instead of the edge
-- node. This will go as soon as decoupling is done; at this point we will need
-- this edge node and the wallet will simply boil down to a webserver, started
-- independently.
startCluster
    :: [(NodeName, NodeType)]
    -> IO (Env, [FilePath], Manager)
startCluster nodes = do
    env0 <- getEnvironment >>= withSystemStart
        . Map.union defaultIntegrationEnv
        . Map.fromList
        . stripFilterPrefix prefix

    let stateDir   = env0 ! "STATE_DIR" -- Safe, we just defaulted it above
    let configFile = env0 ! "CONFIGURATION_FILE" -- Safe, we just defaulted it above
    let configKey  = env0 ! "CONFIGURATION_KEY" -- Safe, we just defaulted it above

    handles <- forM nodes $ \node@(_, nodeType) -> runAsync $ \yield -> do
        let (artifacts, nodeEnv) = prepareEnvironment node nodes stateDir env0
        let (genesis, topology, logger, tls) = artifacts
        case nodeType of
            NodeCore -> do
                void (init genesis >> init topology >> init logger >> init tls)
                yield (nodeEnv, Nothing) >> startNode node nodeEnv
            NodeRelay -> do
                void (init topology >> init logger >> init tls)
                yield (nodeEnv, Nothing) >> startNode node nodeEnv
            NodeEdge -> do
                manager <- init topology >> init logger >> init tls
                yield (nodeEnv, Just manager) >> startWallet node nodeEnv

    (env, manager) <- fmap (Prelude.head . catMaybes) $ forM handles $ \(_, (env, manager)) -> do
        printCartouche env >> return ((env,) <$> manager)

    let configOpts = ConfigurationOptions
            { cfoFilePath    = configFile
            , cfoKey         = toText configKey
            , cfoSystemStart = Just 0
            , cfoSeed        = Nothing
            }
    (env,,manager) <$> getGenesisKeys stateDir configOpts
  where
    init :: Artifact a b -> IO b
    init = initializeArtifact


-- | Start a wallet, which is still a node (decoupling incoming!)
startWallet
    :: (NodeName, NodeType) -- ^ The actual node name
    -> Env                  -- ^ A "simulation" of the system ENV as a 'Map String String'
    -> IO ()
startWallet (NodeName nodeIdT, _) env = do
    nArgs <- parseNodeArgs
    cArgs <- parseCommonNodeArgs
    wArgs <- parseWalletArgs
    let lArgs = getLoggingArgs cArgs
    withCompileInfo $ launchNode nArgs cArgs lArgs (actionWithWallet wArgs)
  where
    parseNodeArgs = do
        let nVars = varFromParser nodeArgsParser
        let nInfo = info nodeArgsParser mempty
        handleParseResult $ execParserEnv env nVars nInfo

    parseCommonNodeArgs = do
        let cVars = varFromParser commonNodeArgsParser
        let cInfo = info commonNodeArgsParser mempty
        handleParseResult $ execParserEnv env cVars cInfo

    parseWalletArgs = do
        let wVars = varFromParser walletBackendParamsParser
        let wInfo = info walletBackendParamsParser mempty
        handleParseResult (execParserEnv env wVars wInfo)

    getLoggingArgs cArgs =
        (loggingParams (fromString $ T.unpack nodeIdT) cArgs)
            { lpConsoleLog = Just False }


-- | Make HttpRequest continuously for a while to wait after the node.
-- This is a temporary, simplified version of what in 'Cardano.Cluster' that
-- works with WalletHttpClient.
waitForNode
    :: WalletHttpClient  -- ^ An Http Client configured against a given node
    -> MaxWaitingTime  -- ^ Maximum waiting time, in seconds
    -> IO ()
waitForNode client (MaxWaitingTime s) = do
    res <- race (threadDelay $ s * oneSecond) retry
    case res of
        Left _ ->
            fail $ "Giving up waiting for node to start: it takes too long"
        Right _ ->
            return ()
  where
    retry :: IO ()
    retry = threadDelay oneSecond >> waitForNode'

    waitForNode' :: IO ()
    waitForNode' = getNodeInfo client NoNtpCheck >>= \case
        Right _ ->
            return ()
        Left (ClientHttpError ConnectionError{}) ->
            retry
        Left err ->
            fail $ "Failed to wait for node to start: " <> show err


-- | Get poor keys
getGenesisKeys
    :: FilePath
    -> ConfigurationOptions
    -> IO [FilePath]
getGenesisKeys stateDir configOpts = do
    gs <- getGeneratedSecrets configOpts
    let genesisKeys =
            [ stateDir </> "generated-keys" </> "poor" </> (show i <> ".key")
            | i <- iterate (+1) (0 :: Int)
            ]
    return $ take (length $ gsPoorSecrets gs) genesisKeys
  where
    getGeneratedSecrets :: ConfigurationOptions -> IO GeneratedSecrets
    getGeneratedSecrets opts = fst <$>
        ( usingNamedPureLogger "_"
            $ withConfigurations Nothing Nothing False opts
            $ \config _ _ _ -> configGeneratedSecretsThrow config
        )


-- | Some debugging output upon starting a cluster
printCartouche :: Env -> IO ()
printCartouche env = do
    let colSize = 35
    putTextLn $ toText (env ! "NODE_ID") <> T.replicate (colSize - length (env !  "NODE_ID")) "-"
    when (Map.member "LISTEN" env) $ putTextLn $  "|.....listen:        " <> toText (env ! "LISTEN")
    putTextLn $ "|.....api address:   " <> toText (env ! "NODE_API_ADDRESS")
    putTextLn $ "|.....doc address:   " <> toText (env ! "NODE_DOC_ADDRESS")
    putTextLn $ "|.....system start:  " <> toText (env ! "SYSTEM_START")
    putTextLn $ T.replicate colSize "-" <> "\n"
