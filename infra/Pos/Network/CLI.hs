{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- following Pos.Util.UserSecret
#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

-- | Command line interface for specifying the network config
module Pos.Network.CLI (
    NetworkConfigOpts(..)
  , NetworkConfigException(..)
  , networkConfigOption
  , ipv4ToNodeId
  , intNetworkConfigOpts
    -- * Exported primilary for testing
  , readTopology
  , fromPovOf
  ) where

import           Control.Concurrent
import           Control.Exception               (Exception (..), try)
import qualified Data.ByteString.Char8           as BS.C8
import           Data.IP                         (IPv4)
import qualified Data.Map.Strict                 as M
import           Data.Maybe                      (fromJust)
import qualified Data.Yaml                       as Yaml
import           Formatting                      (sformat, shown, (%))
import           Mockable                        (Mockable, fork)
import           Mockable.Concurrent
import           Network.Broadcast.OutboundQueue (Alts, Peers, peersFromList)
import qualified Network.DNS                     as DNS
import qualified Options.Applicative             as Opt
import qualified Pos.DHT.Real.Param              as DHT (KademliaParams,
                                                         MalformedDHTKey (..),
                                                         fromYamlConfig)
import           Pos.Network.DnsDomains          (DnsDomains (..), NodeAddr (..))
import           Pos.Network.Types               (NodeId)
import qualified Pos.Network.Types               as T
import           Pos.Network.Yaml                (NodeMetadata (..), NodeName (..))
import qualified Pos.Network.Yaml                as Y
import           Pos.Util.TimeWarp               (addressToNodeId)
import           System.Wlog.CanLog              (WithLogger, logError, logNotice)
import           Universum

#ifdef POSIX
import           Pos.Util.SigHandler             (Signal (..), installHandler)
#endif

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NetworkConfigOpts = NetworkConfigOpts {
      -- | Filepath to .yaml file with the network topology
      networkConfigOptsTopology :: Maybe FilePath

    , networkConfigOptsKademlia :: Maybe FilePath

      -- | Name of the current node
    , networkConfigOptsSelf     :: Maybe NodeName

      -- | Port number to use when translating IP addresses to NodeIds
    , networkConfigOptsPort     :: Word16
    }
  deriving (Show)

{-------------------------------------------------------------------------------
  Parser
-------------------------------------------------------------------------------}

networkConfigOption :: Opt.Parser NetworkConfigOpts
networkConfigOption = NetworkConfigOpts
    <$> (optional . Opt.strOption $ mconcat [
            Opt.long "topology"
          , Opt.metavar "FILEPATH"
          , Opt.help "Path to a YAML file containing the network topology"
          ])
    <*> (optional . Opt.strOption $ mconcat [
            Opt.long "kademlia"
          , Opt.metavar "FILEPATH"
          , Opt.help "Path to a YAML file containing the kademlia configuration"
          ])
    <*> (optional . Opt.option (fromString <$> Opt.str) $ mconcat [
            Opt.long "node-id"
          , Opt.metavar "NODE_ID"
          , Opt.help "Identifier for this node within the network"
          ])
    <*> (Opt.option Opt.auto $ mconcat [
            Opt.long "default-port"
          , Opt.metavar "PORT"
          , Opt.help "Port number for IP address to node ID translation"
          , Opt.value 3000
          ])

{-------------------------------------------------------------------------------
  Defaults
-------------------------------------------------------------------------------}

-- | The topology we assume when no topology file is specified
defaultTopology :: Y.Topology
defaultTopology = Y.TopologyBehindNAT defaultDnsDomains

-- | The default DNS domains used for relay discovery
--
-- TODO: Give this a proper value
defaultDnsDomains :: DnsDomains DNS.Domain
defaultDnsDomains = DnsDomains [
      [NodeAddrDNS "todo.defaultDnsDomain.com" Nothing]
    ]

{-------------------------------------------------------------------------------
  Monitor for static peers
-------------------------------------------------------------------------------}

data MonitorEvent =
    MonitorRegister (Peers NodeId -> IO ())
  | MonitorSIGHUP

-- | Monitor for changes to the static config
monitorStaticConfig :: forall m. (WithLogger m, MonadIO m, Mockable Fork m)
                    => NetworkConfigOpts
                    -> T.NodeType   -- ^ Our node type
                    -> Peers NodeId -- ^ Initial value
                    -> m T.StaticPeers
monitorStaticConfig cfg@NetworkConfigOpts{..} ourNodeType initPeers = do
    events :: Chan MonitorEvent <- liftIO $ newChan

    let loop :: Peers NodeId -> [Peers NodeId -> IO ()] -> m ()
        loop peers handlers = do
          event <- liftIO $ readChan events
          case event of
            MonitorRegister handler -> do
              runHandler peers handler -- Call new handler with current value
              loop peers (handler:handlers)
            MonitorSIGHUP -> do
              let fp = fromJust networkConfigOptsTopology
              mParsedTopology <- liftIO $ try $ readTopology fp
              case mParsedTopology of
                Right (Y.TopologyStatic allPeers) -> do
                  (nodeType, newPeers) <- liftIO $ fromPovOf cfg allPeers
                  unless (nodeType == ourNodeType) $ logError $ changedType fp
                  mapM_ (runHandler newPeers) handlers
                  logNotice $ sformat "SIGHUP: Re-read topology"
                  loop newPeers handlers
                Right _otherTopology -> do
                  logError $ changedFormat fp
                  loop peers handlers
                Left ex -> do
                  logError $ readFailed fp ex
                  loop peers handlers

#ifdef POSIX
    liftIO $ installHandler SigHUP $ writeChan events MonitorSIGHUP
#endif

    _tid <- fork $ loop initPeers []
    return T.StaticPeers {
        T.staticPeersOnChange = writeChan events . MonitorRegister
      }
  where
    runHandler :: Peers NodeId -> (Peers NodeId -> IO ()) -> m ()
    runHandler peers handler = do
        mu <- liftIO $ try (handler peers)
        case mu of
          Left  ex -> logError $ handlerError ex
          Right () -> return ()

    changedFormat, changedType :: FilePath -> Text
    changedFormat = sformat $ "SIGHUP: The topology type defined in " % shown % " changed. Ignored."
    changedType   = sformat $ "SIGHUP: Our node type defined in "     % shown % " changed. Ignored."

    readFailed :: FilePath -> SomeException -> Text
    readFailed = sformat $ "SIGHUP: Failed to read " % shown % ": " % shown % ". Ignored."

    handlerError :: SomeException -> Text
    handlerError = sformat $ "Exception thrown by staticPeersOnChange handler: " % shown % ". Ignored."

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpreter for the network config opts
intNetworkConfigOpts :: forall m. (WithLogger m, MonadIO m, Mockable Fork m)
                     => NetworkConfigOpts
                     -> m (T.NetworkConfig DHT.KademliaParams)
intNetworkConfigOpts cfg@NetworkConfigOpts{..} = do
    parsedTopology <- case networkConfigOptsTopology of
                        Nothing -> return defaultTopology
                        Just fp -> liftIO $ readTopology fp
    ourTopology <- case parsedTopology of
      Y.TopologyStatic allPeers -> do
        (nodeType, initPeers) <- liftIO $ fromPovOf cfg allPeers
        staticPeers <- monitorStaticConfig cfg nodeType initPeers
        case nodeType of
          T.NodeCore ->
            return $ T.TopologyCore staticPeers
          T.NodeRelay -> do
            kparams <- liftIO $ getKademliaParams cfg
            return $ T.TopologyRelay staticPeers kparams
          T.NodeEdge ->
            -- This will never happen. Either our node name is not found in
            -- the table, or it's a core or relay.
            liftIO $ throwM NetworkConfigSelfEdge
      Y.TopologyBehindNAT dnsDomains ->
        return $ T.TopologyBehindNAT dnsDomains
      Y.TopologyP2P v f -> do
        kparams <- liftIO $ getKademliaParams cfg
        return (T.TopologyP2P v f kparams)
      Y.TopologyTraditional v f -> do
        kparams <- liftIO $ getKademliaParams cfg
        return (T.TopologyTraditional v f kparams)
    return T.NetworkConfig {
        ncTopology    = ourTopology
      , ncDefaultPort = networkConfigOptsPort
      , ncSelfName    = networkConfigOptsSelf
      }

-- | Come up with kademlia parameters, possibly throwing an exception in case
-- there's no configuration file path given, or if it couldn't be parsed.
getKademliaParams :: NetworkConfigOpts
                  -> IO DHT.KademliaParams
getKademliaParams cfg = case networkConfigOptsKademlia cfg of
    Nothing -> throwM MissingKademliaConfig
    Just fp -> do
      kconf <- parseKademlia fp
      either (throwM . DHT.MalformedDHTKey) return (DHT.fromYamlConfig kconf)

-- | Perspective on 'AllStaticallyKnownPeers' from the point of view of
-- a single node
fromPovOf :: NetworkConfigOpts
          -> Y.AllStaticallyKnownPeers
          -> IO (T.NodeType, Peers NodeId)
fromPovOf cfg@NetworkConfigOpts{..} allPeers =
    case networkConfigOptsSelf of
      Nothing   -> throwM NetworkConfigSelfUnknown
      Just self -> do
        -- TODO: Do we want to allow to override the DNS config?
        resolvSeed <- DNS.makeResolvSeed DNS.defaultResolvConf
        DNS.withResolver resolvSeed $ \resolver -> do
          selfMetadata <- metadataFor allPeers self
          selfPeers    <- mkPeers resolver (Y.nmRoutes selfMetadata)
          let selfType = Y.nmType selfMetadata
          return (selfType, peersFromList selfPeers)
  where
    mkPeers :: DNS.Resolver -> Y.NodeRoutes -> IO [(T.NodeType, Alts NodeId)]
    mkPeers resolver (Y.NodeRoutes routes) = mapM (mkAlts resolver) routes

    mkAlts :: DNS.Resolver -> Alts NodeName -> IO (T.NodeType, Alts NodeId)
    mkAlts _        []    = throwM $ EmptyListOfAltsFor (fromJust networkConfigOptsSelf)
    mkAlts resolver names = do
      alts@(firstAlt:_) <- mapM (metadataFor allPeers) names
      let altsType = nmType firstAlt -- assume all alts have same type
      (altsType,) <$> mapM (resolveNodeAddr cfg resolver)
                           (zip names $ map nmAddress alts)

-- | Resolve node name to IP address
--
-- We do this when reading the topology file so that we detect DNS problems
-- early, and so that we are using canonical addresses (IP addresses) for
-- node IDs.
--
-- NOTE: This is /only/ used for core or edge nodes (nodes with a statically
-- configured set of peers). For such nodes it makes sense to detect DNS
-- problems at startup. For behind NAT nodes we do not do any resolution at
-- this point; instead, it happens dynamically in the subscription worker.
-- This is important; in user applications we certainly don't want to prevent
-- the application from starting up because of DNS problems at startup.
--
-- TODO: Support re-reading this file after SIGHUP.
resolveNodeAddr :: NetworkConfigOpts
                -> DNS.Resolver
                -> (NodeName, NodeAddr (Maybe DNS.Domain))
                -> IO NodeId
resolveNodeAddr cfg _ (_, NodeAddrExact addr mPort) = do
    let port = fromMaybe (networkConfigOptsPort cfg) mPort
    return $ addressToNodeId (addr, port)
resolveNodeAddr cfg resolver (name, NodeAddrDNS mHost mPort) = do
    let host = fromMaybe (nameToDomain name)         mHost
        port = fromMaybe (networkConfigOptsPort cfg) mPort

    mAddrs <- DNS.lookupA resolver host
    case mAddrs of
      Left err            -> throwM $ NetworkConfigDnsError host err
      Right []            -> throwM $ CannotResolve name
      Right addrs@(_:_:_) -> throwM $ NoUniqueResolution name addrs
      Right [addr]        -> return $ ipv4ToNodeId addr port
  where
    nameToDomain :: NodeName -> DNS.Domain
    nameToDomain (NodeName n) = BS.C8.pack (toString n)

ipv4ToNodeId :: IPv4 -> Word16 -> NodeId
ipv4ToNodeId addr port = addressToNodeId (BS.C8.pack (show addr), port)

metadataFor :: Y.AllStaticallyKnownPeers -> NodeName -> IO Y.NodeMetadata
metadataFor (Y.AllStaticallyKnownPeers allPeers) node =
    case M.lookup node allPeers of
      Nothing       -> throwM $ UndefinedNodeName node
      Just metadata -> return metadata

readTopology :: FilePath -> IO Y.Topology
readTopology fp = do
    mTopology <- Yaml.decodeFileEither fp
    case mTopology of
      Left  err      -> throwM $ CannotParseNetworkConfig err
      Right topology -> return topology

parseKademlia :: FilePath -> IO Y.KademliaParams
parseKademlia fp = do
    mKademlia <- Yaml.decodeFileEither fp
    case mKademlia of
      Left  err      -> throwM $ CannotParseKademliaConfig err
      Right kademlia -> return kademlia

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Something is wrong with the network configuration
--
-- NOTE: Behind NAT nodes are not given an explicit network configuration file,
-- but instead rely on the default topology. These exceptions should never be
-- thrown for behind NAT nodes, as we don't want to prevent the user application
-- from starting up.
data NetworkConfigException =
    -- | We cannot parse the topology .yaml file
    CannotParseNetworkConfig Yaml.ParseException

    -- | A Kademlia configuration file is expected but was not specified.
  | MissingKademliaConfig

    -- | We cannot parse the kademlia .yaml file
  | CannotParseKademliaConfig Yaml.ParseException

    -- | We use a set of statically known peers but we weren't given the
    -- name of the current node
  | NetworkConfigSelfUnknown

    -- | We resolved our name under static configuration to be an edge node.
    -- This indicates a programmer error.
  | NetworkConfigSelfEdge

    -- | The .yaml file contains a node name which is undefined
  | UndefinedNodeName NodeName

    -- | The static routes for a node contains an empty list of alternatives
  | EmptyListOfAltsFor NodeName

    -- | Something went wrong during node name resolution
  | NetworkConfigDnsError DNS.Domain DNS.DNSError

    -- | Could not resolve a node name to an IP address
  | CannotResolve NodeName

    -- | DNS returned multiple IP addresses for the give node name
    --
    -- This is no good because we need canonical node IDs.
  | NoUniqueResolution NodeName [IPv4]
  deriving (Show)

instance Exception NetworkConfigException
