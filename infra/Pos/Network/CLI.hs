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

import           Universum
import           Data.IP (IPv4)
import           Network.Broadcast.OutboundQueue (Alts, peersFromList)
import           Pos.Network.Types (NodeId)
import           Pos.Network.Yaml (NodeName(..), NodeMetadata(..), NodeAddr(..))
import           Pos.Network.DnsDomains (DnsDomains(..))
import           Pos.Util.TimeWarp (addressToNodeId)
import qualified Data.ByteString.Char8      as BS.C8
import qualified Data.Map.Strict            as M
import qualified Data.Text                  as Text
import qualified Data.Yaml                  as Yaml
import qualified Network.DNS                as DNS
import qualified Options.Applicative.Simple as Opt
import qualified Pos.Network.Types          as T
import qualified Pos.Network.Yaml           as Y

{-------------------------------------------------------------------------------
  Command line arguments
-------------------------------------------------------------------------------}

data NetworkConfigOpts = NetworkConfigOpts {
      -- | Filepath to .yaml file with the network topology
      networkConfigOptsTopology :: Maybe FilePath

      -- | Name of the current node
    , networkConfigOptsSelf :: Maybe NodeName

      -- | Port number to use when translating IP addresses to NodeIds
    , networkConfigOptsPort :: Word16
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
defaultDnsDomains :: DnsDomains
-- TODO: Give this a proper value
defaultDnsDomains = DnsDomains [["todo.defaultDnsDomain.com"]]

{-------------------------------------------------------------------------------
  Interpreter
-------------------------------------------------------------------------------}

-- | Interpreter for the network config opts
intNetworkConfigOpts :: NetworkConfigOpts -> IO T.NetworkConfig
intNetworkConfigOpts cfg@NetworkConfigOpts{..} = do
    parsedTopology <- case networkConfigOptsTopology of
                        Nothing -> return defaultTopology
                        Just fp -> readTopology fp
    ourTopology <- case parsedTopology of
      Y.TopologyStatic allStaticallyKnownPeers ->
        fromPovOf cfg allStaticallyKnownPeers networkConfigOptsSelf
      Y.TopologyBehindNAT dnsDomains ->
        return $ T.TopologyBehindNAT dnsDomains
      Y.TopologyP2P ->
        return $ T.TopologyP2P
      Y.TopologyTransitional ->
        return $ T.TopologyTransitional
    return T.NetworkConfig {
        ncTopology    = ourTopology
      , ncDefaultPort = networkConfigOptsPort
      , ncSelfName    = networkConfigOptsSelf
      }

-- | Perspective on 'AllStaticallyKnownPeers' from the point of view of
-- a single node
fromPovOf :: NetworkConfigOpts
          -> Y.AllStaticallyKnownPeers
          -> Maybe NodeName
          -> IO T.Topology
fromPovOf _ _ Nothing =
    throwM NetworkConfigSelfUnknown
fromPovOf cfg allPeers (Just self) = do
    -- TODO: Do we want to allow to override the DNS config?
    resolvSeed <- DNS.makeResolvSeed DNS.defaultResolvConf
    DNS.withResolver resolvSeed $ \resolver -> do
      selfMetadata <- metadataFor allPeers self
      selfPeers    <- mkPeers resolver (Y.nmRoutes selfMetadata)
      let selfType = Y.nmType selfMetadata
      return $ T.TopologyStatic selfType (peersFromList selfPeers)
  where
    mkPeers :: DNS.Resolver -> Y.NodeRoutes -> IO [(T.NodeType, Alts NodeId)]
    mkPeers resolver (Y.NodeRoutes routes) = mapM (mkAlts resolver) routes

    mkAlts :: DNS.Resolver -> Alts NodeName -> IO (T.NodeType, Alts NodeId)
    mkAlts _        []    = throwM $ EmptyListOfAltsFor self
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
resolveNodeAddr :: NetworkConfigOpts
                -> DNS.Resolver
                -> (NodeName, NodeAddr)
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
    nameToDomain (NodeName n) = BS.C8.pack (Text.unpack n)

ipv4ToNodeId :: IPv4 -> Word16 -> NodeId
ipv4ToNodeId addr port = addressToNodeId (BS.C8.pack (show addr), port)

metadataFor :: Y.AllStaticallyKnownPeers -> NodeName -> IO Y.NodeMetadata
metadataFor (Y.AllStaticallyKnownPeers allStaticallyKnownPeers) node =
    case M.lookup node allStaticallyKnownPeers of
      Nothing       -> throwM $ UndefinedNodeName node
      Just metadata -> return metadata

readTopology :: FilePath -> IO Y.Topology
readTopology fp = do
    mTopology <- Yaml.decodeFileEither fp
    case mTopology of
      Left  err      -> throwM $ CannotParseNetworkConfig err
      Right topology -> return topology

{-------------------------------------------------------------------------------
  Errors
-------------------------------------------------------------------------------}

-- | Something is wrong with the network configuration
data NetworkConfigException =
    -- | We cannot parse the .yaml file
    CannotParseNetworkConfig Yaml.ParseException

    -- | We use a set of statically known peers but we weren't given the
    -- name of the current node
  | NetworkConfigSelfUnknown

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
