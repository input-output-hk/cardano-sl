{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeApplications  #-}

module NodeControl (NodeHandle, startNode, stopNode, stopNodeByName, genSystemStart, mkTopo, keygen, NodeInfo(..), mutateConfigurationYaml, createNodes, cleanupNodes) where

import           Control.Concurrent.Async.Lifted.Safe
import           Control.Concurrent.STM.TVar (modifyTVar)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BSL
import           Data.Ix (range)
import qualified Data.Map as Map
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import           Data.Time (NominalDiffTime, addUTCTime, defaultTimeLocale,
                     formatTime, getCurrentTime)
import           Data.Time.Units (Second, convertUnit)
import qualified Data.Yaml as Y
import           Network.Broadcast.OutboundQueue
                     (MaxBucketSize (BucketSizeUnlimited))
import           Network.DNS.Types (Domain)
import           PocMode (PocMode, acStatePath, acTopology, nodeHandles)
import qualified Pos.Client.CLI as CLI
import           Pos.Core.Slotting (Timestamp, getTimestamp)
import           Pos.Infra.Network.DnsDomains (NodeAddr (..))
import           Pos.Infra.Network.Types (Fallbacks, NodeName (..),
                     NodeType (..), Valency)
import           Pos.Infra.Network.Yaml (AllStaticallyKnownPeers (..),
                     DnsDomains (..), NodeMetadata (..), NodeRegion (..),
                     NodeRoutes (..), Topology (..))
import           Pos.Launcher (Configuration, ConfigurationOptions,
                     cfoFilePath_L, cfoSystemStart_L)
import           Pos.Util.Config (parseYamlConfig)
import           Pos.Util.Wlog (logInfo, logWarning)
import           System.Posix.Signals
import           System.Process
import           Types
import           Universum

data NodeInfo = NodeInfo
            { niIndex       :: Integer
            , niType        :: Types.NodeType
            , stateRoot     :: Text
            , topoPath      :: Text
            , niCfgFilePath :: CLI.CommonArgs
            }

startingPortOffset :: Num i => Types.NodeType -> i
startingPortOffset Core  = 100
startingPortOffset Relay = 0

mkTopo :: Integer -> Integer -> Topology
mkTopo cores relays = do
  let
    nmRegion = NodeRegion "none"
    nmSubscribe :: DnsDomains ByteString
    nmSubscribe = DnsDomains []
    nmValency :: Valency
    nmValency = 1
    nmFallbacks :: Fallbacks
    nmFallbacks = 1
    nmKademlia = False
    nmPublicDNS = False
    nmMaxSubscrs = BucketSizeUnlimited
    mkNodeMeta :: Integer -> Types.NodeType -> NodeMetadata
    mkNodeMeta idx typ = do
      let
        nmType = case typ of
          Core  -> NodeCore
          Relay -> NodeRelay
        nmAddress :: NodeAddr (Maybe Domain)
        nmAddress = NodeAddrExact "127.0.0.1" (Just $ startingPortOffset typ + 3000 + (fromIntegral idx))
        mkRoute :: Integer -> [ NodeName ]
        mkRoute x = [ NodeName ("core-" <> show x) ]
        nmRoutes = case typ of
          Core  -> NodeRoutes [ [ NodeName "relay-0" ] ]
          Relay -> NodeRoutes $ map mkRoute $ range (0,cores)
      NodeMetadata{..}
    mkCoreTup :: Integer -> (NodeName, NodeMetadata)
    mkCoreTup idx = (NodeName $ T.pack $ "core-" <> (show idx), mkNodeMeta idx Core)
    mkRelayTup idx = (NodeName $ T.pack $ "relay-" <> (show idx), mkNodeMeta idx Relay)
    allCoreNodes :: [ (NodeName, NodeMetadata) ]
    allCoreNodes = map mkCoreTup (range (0, cores))
    allRelayNodes :: [ (NodeName, NodeMetadata) ]
    allRelayNodes = map mkRelayTup (range (0, relays))
  TopologyStatic $ AllStaticallyKnownPeers $ M.fromList (allCoreNodes <> allRelayNodes)

typeToString :: Types.NodeType -> String
typeToString Core  = "core"
typeToString Relay = "relay"

commonNodeParams :: NodeInfo -> [ String ]
commonNodeParams (NodeInfo idx typ stateRoot topoPath cfg) = [
    "--configuration-file", cfg ^. CLI.configurationOptions_L . cfoFilePath_L
  , "--topology", T.unpack topoPath
  , "--db-path", (T.unpack stateRoot) <> "/test-state/" <> (typeToString typ) <> (show idx) <> "-db"
  , "--node-id", (typeToString typ) <> "-" <> (show idx)
  , "--node-api-address", "127.0.0.1:" <> show (startingPortOffset typ + 8083 + idx)
  , "--no-tls"
  , "--node-doc-address", "127.0.0.1:" <> show (startingPortOffset typ + 8180 + idx)
  , "--json-log", "test-state/" <> typeToString typ <> show idx <> ".json"
  , "--logs-prefix", "test-state/logs-" <> typeToString typ <> show idx
  ] <> (maybeSystemStart $ cfg ^. CLI.configurationOptions_L . cfoSystemStart_L)
  <> (maybeLogConfig $ cfg ^. CLI.logConfig_L)

maybeSystemStart :: Maybe Timestamp -> [ String ]
maybeSystemStart Nothing = []
maybeSystemStart (Just ts) = [ "--system-start", show seconds ]
  where
    seconds :: Integer
    seconds = fromIntegral @Second (convertUnit $ getTimestamp ts)

maybeLogConfig :: Maybe FilePath -> [ String ]
maybeLogConfig Nothing          = []
maybeLogConfig (Just logconfig) = [ "--log-config", logconfig ]

commonNodeStart :: String -> [ String ] -> Types.NodeType -> Integer -> PocMode ()
commonNodeStart prog args typ idx = do
  let
    typename = typeToString typ
  childStdout <- openFile ("test-state/" <> typename <> "-stdout-" <> show idx) AppendMode
  let
    pc :: CreateProcess
    pc = (proc prog args) { std_out = UseHandle childStdout }
  (_stdin, _stdout, _stderr, ph) <- liftIO $ createProcess pc
  later <- liftIO $ async $ do
    _ <- waitForProcess ph
    pure ()
  let
    hnd = NodeHandle later ph
  tvar <- nodeHandles
  atomically $ modifyTVar tvar $ Map.insert (typ, idx) hnd

startNode :: NodeInfo -> PocMode ()
startNode info@(NodeInfo idx typ stateRoot _topoPath _cfg) =
    commonNodeStart "cardano-node-simple" params typ idx
  where
    params = (commonNodeParams info) <> nonSharedParams <> sharedParams
    nonSharedParams = case typ of
      Core  -> [ "--keyfile", T.unpack (stateRoot <> "/genesis-keys/generated-keys/rich/key" <> (show idx) <> ".sk")]
      Relay -> [ "--keyfile", T.unpack (stateRoot <> "/relay" <> (show idx) <> ".sk")]
    sharedParams = [ "--listen", "127.0.0.1:" <> show (startingPortOffset typ + idx + 3000)
                   , "--tlskey", "scripts/tls-files/server.key"
                   , "--tlsca", "scripts/tls-files/ca.crt"
                   , "--tlscert", "scripts/tls-files/server.crt"
                   ]

stopNode :: NodeHandle -> IO ()
stopNode (NodeHandle _async ph) = do
  maybePid <- getPid ph
  case maybePid of
    Just pid -> do
      signalProcess sigINT pid
    Nothing -> do
      logWarning "node already stopped when trying to stop it"

stopNodeByName :: (Types.NodeType, Integer) -> PocMode ()
stopNodeByName name = do
  map' <- nodeHandles >>= atomically . readTVar
  case (Map.lookup name map') of
    Just hnd -> liftIO $ stopNode hnd
    Nothing  -> logWarning ("node " <> show name <> " not found in node map")

genSystemStart :: NominalDiffTime -> IO String
genSystemStart offset = formatTime defaultTimeLocale "%s" . addUTCTime offset <$> getCurrentTime

keygen :: ConfigurationOptions -> Text -> IO ()
keygen cfg stateRoot = do
  let
    params = [ "generate-keys-by-spec"
             , "--genesis-out-dir", T.unpack (stateRoot <> "/genesis-keys")
             , "--configuration-file", cfg ^. cfoFilePath_L
             ] <> (maybeSystemStart $ cfg ^. cfoSystemStart_L)
    pc :: CreateProcess
    pc = proc "cardano-keygen" params
  (_stdin, _stdout, _stderr, ph) <- createProcess pc
  _ <- waitForProcess ph
  pure ()

mutateConfigurationYaml :: FilePath -> Text -> (Configuration -> Configuration) -> IO ByteString
mutateConfigurationYaml filepath key mutator = do
  cfg <- parseYamlConfig filepath key
  let
    newcfg = mutator cfg
    newmap = Map.singleton key newcfg
    yaml = Y.encode newmap
  pure yaml

createNodes :: ScriptRuntimeParams -> ScriptRunnerOptions -> PocMode ()
createNodes srp opts = do
  topo <- view acTopology
  stateDir <- view acStatePath
  let
    path = stateDir <> "/topology.yaml"
    -- the config for the script-runner is mirrored to the nodes it starts
    cfg = opts ^. srCommonNodeArgs . CLI.commonArgs_L
  liftIO $ do
    BSL.writeFile (T.unpack path) (A.encode topo)
    keygen (cfg ^. CLI.configurationOptions_L)  stateDir
  forM_ (range (0, srpCoreNodes srp - 1)) $
    \node -> startNode (NodeInfo (fromIntegral node) Core stateDir path cfg)
  forM_ (range (0,0)) $ \node -> startNode (NodeInfo node Relay stateDir path cfg)

cleanupNodes :: PocMode ()
cleanupNodes = do
  logInfo "stopping all nodes"
  nodeHandles >>= atomically . readTVar >>= liftIO . mapM_ stopNode
