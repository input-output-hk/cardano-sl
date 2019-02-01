{-| Demo cluster of nodes. See cluster/README.md -}

module Cardano.Cluster
    (
    -- * Types
      NodeName (..)
    , NodeType (..)
    , RunningNode (..)
    , mkNamedNodes

    -- * Start Cluster
    , startCluster
    , startNode

    -- * Monitor cluster
    , MaxWaitingTime (..)
    , waitForNode
    ) where

import           Universum hiding (init)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, race)
import           Control.Lens (at)
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import           Options.Applicative (handleParseResult, info)
import           Servant.Client (ServantError (..))
import           System.Environment (getEnvironment)

import           Cardano.Cluster.Environment (Artifact (..), Env,
                     prepareEnvironment, withStateDirectory, withSystemStart)
import           Cardano.Cluster.Util (execParserEnv, oneSecond, runAsync,
                     stripFilterPrefix, varFromParser)
import           Cardano.Node.API (launchNodeServer)
import           Cardano.Node.Client (ClientError (..), NodeClient (..),
                     NodeHttpClient)
import           Cardano.Node.Manager (Manager)
import           Pos.Chain.Update (updateConfiguration)
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeApiArgsParser, nodeArgsParser)
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Infra.Network.Types (NodeName (..), NodeType (..))
import           Pos.Launcher (LoggingParams (..), actionWithCoreNode,
                     launchNode)
import           Pos.Node.API (ForceNtpCheck (..), NodeInfo (..),
                     SyncPercentage, mkSyncPercentage)
import           Pos.Util.CompileInfo (compileInfo, withCompileInfo)
import           Pos.Util.Wlog (logInfo)


-- | A type representing a running node. The process is captured within the
-- 'Async' handle. For edges nodes, there's an exta connection manager configured
-- to talk to the underlying node API.
data RunningNode
    = RunningNode NodeType NodeName Env Manager (Async ())


-- | Start a cluster of nodes in different threads.
-- Nodes get their (optional) arguments from the ENV.
--
-- For more details, look at cluster/README.md
startCluster
    :: String                 -- ^ A prefix. Only ENV vars with this prefix will be considered
    -> [(NodeName, NodeType)] -- ^ A list of node names forming the cluster
    -> IO [RunningNode]
startCluster prefix nodes = do
    env <- (withSystemStart . Map.fromList . stripFilterPrefix prefix) =<< getEnvironment
    mvar <- newMVar ()
    let once io = tryTakeMVar mvar >>= \case Nothing -> return (); Just _ -> void io
    handles <- forM nodes $ \node@(nodeId, nodeType) -> runAsync $ \yield ->
        withStateDirectory (env ^. at "STATE_DIR") $ \stateDir -> do
            let (artifacts, nodeEnv) = prepareEnvironment node nodes stateDir env
            let (genesis, topology, logger, tls) = artifacts

            when (nodeType == NodeCore) $ once (init genesis)
            manager <- init topology >> init logger >> init tls
            yield (RunningNode nodeType nodeId nodeEnv manager)

            startNode node nodeEnv

    return $ map (\(h, running) -> running h) handles
  where
    init :: Artifact a b -> IO b
    init = initializeArtifact


-- | Start a demo node using the given environment as a context.
-- This action never returns, unless the node crashes.
startNode
    :: (NodeName, NodeType) -- ^ The actual node name
    -> Env                  -- ^ A "simulation" of the system ENV as a 'Map String String'
    -> IO ()
startNode (NodeName nodeIdT, _) env = do
    nArgs <- parseNodeArgs
    cArgs <- parseCommonNodeArgs
    aArgs <- parseApiArgs
    let lArgs = getLoggingArgs cArgs

    let nodeServer = case aArgs of
            Nothing ->
                \_ _ _ _ _ _ -> logInfo "Monitoring API is disabled."
            Just args ->
                launchNodeServer args

    withCompileInfo $ launchNode nArgs cArgs lArgs $ \genC walC txpC ntpC nodC sscC resC -> do
        actionWithCoreNode
            (nodeServer ntpC resC updateConfiguration compileInfo genC)
            genC walC txpC ntpC nodC sscC resC
  where
    parseApiArgs = do
        let aVars = varFromParser nodeApiArgsParser
        let aInfo = info nodeApiArgsParser mempty
        handleParseResult $ execParserEnv env aVars aInfo

    parseNodeArgs = do
        let nVars = varFromParser nodeArgsParser
        let nInfo = info nodeArgsParser mempty
        handleParseResult $ execParserEnv env nVars nInfo

    parseCommonNodeArgs = do
        let cVars = varFromParser commonNodeArgsParser
        let cInfo = info commonNodeArgsParser mempty
        handleParseResult $ execParserEnv env cVars cInfo

    -- NOTE
    -- Logging to the console is disabled. This is just noise when multiple
    -- nodes are running at the same time. Logs are available in the logfiles
    -- inside the state directory anyway. `tail -f` is a friend.
    getLoggingArgs cArgs = (loggingParams (fromString $ T.unpack nodeIdT) cArgs)
        { lpConsoleLog = Just False }


-- | Maximum time, in second, a function should for something
newtype MaxWaitingTime = MaxWaitingTime Int deriving (Eq, Show)


-- | Make HttpRequest continuously for a while to wait after the node
waitForNode
    :: NodeHttpClient  -- ^ An Http Client configured against a given node
    -> MaxWaitingTime  -- ^ Maximum waiting time, in seconds
    -> (Maybe SyncPercentage -> IO ())
    -> IO ()
waitForNode client (MaxWaitingTime s) reportProgress = do
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
    waitForNode' = runExceptT (getNodeInfo client NoNtpCheck) >>= \case
        Right success -> do
            let progress = nfoSyncProgress success
            when (progress < mkSyncPercentage 100) $
                reportProgress (Just progress) >> retry

        Left (ErrFromServant ConnectionError{}) ->
            reportProgress Nothing >> retry

        Left err ->
            fail $ "Failed to wait for node to start: " <> show err


-- | Create a list of named nodes of the given type
mkNamedNodes :: NodeType -> Int -> [(NodeName, NodeType)]
mkNamedNodes typ n =
    if n == 1 then
        [ (NodeName (toNodeName typ), typ) ]
    else
        zip
            [ NodeName (toNodeName typ <> show k) | k <- iterate (+1) (0 :: Int) ]
            (replicate n typ)
  where
    toNodeName NodeCore  = "core"
    toNodeName NodeRelay = "relay"
    toNodeName NodeEdge  = "edge"
