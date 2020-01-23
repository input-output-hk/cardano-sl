{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

-- following Pos.Util.UserSecret
#if !defined(mingw32_HOST_OS)
#define POSIX
#endif

-- | Command line interface for specifying the network config
module Pos.Infra.Network.CLI
       (
         NetworkConfigOpts(..)
       , NetworkConfigException(..)
       , networkConfigOption
       , externalNetworkAddressOption
       , listenNetworkAddressOption
       , ipv4ToNetworkAddress
       , intNetworkConfigOpts
       , launchStaticConfigMonitoring
         -- * Exported primarily for testing
       , readTopology
       , readPolicies
       , fromPovOf
       ) where

import           Universum

import           Control.Concurrent (Chan, newChan, readChan, writeChan)
import           Control.Exception (throwIO)
import           Control.Exception.Safe (try)
import qualified Data.ByteString.Char8 as BS.C8
import           Data.IP (IPv4)
import qualified Data.Map.Strict as M
import           Data.Maybe (fromJust, mapMaybe)
import qualified Data.Yaml as Yaml
import           Formatting (build, sformat, shown, (%))
import           Network.Broadcast.OutboundQueue (Alts, Peers, peersFromList)
import qualified Network.DNS as DNS
import qualified Network.Transport.TCP as TCP
import qualified Options.Applicative as Opt

import           Pos.Core.NetworkAddress (NetworkAddress, addrParser,
                     addrParserNoWildcard)
import           Pos.Infra.Network.DnsDomains (DnsDomains (..), NodeAddr (..))
import           Pos.Infra.Network.Types (NodeId, NodeName (..))
import qualified Pos.Infra.Network.Types as T
import           Pos.Infra.Network.Yaml (NodeMetadata (..))
import qualified Pos.Infra.Network.Yaml as Y
import           Pos.Infra.Util.TimeWarp (addressToNodeId)
import           Pos.Util.OptParse (fromParsec)
import           Pos.Util.Trace (Severity (Error, Notice), Trace, traceWith)

#ifdef POSIX
import           Pos.Infra.Util.SigHandler (Signal (..), installHandler)
#endif

----------------------------------------------------------------------------
-- Command line arguments
----------------------------------------------------------------------------

data NetworkConfigOpts = NetworkConfigOpts
    { ncoTopology        :: !(Maybe FilePath)
      -- ^ Filepath to .yaml file with the network topology
    , ncoSelf            :: !(Maybe NodeName)
      -- ^ Name of the current node
    , ncoPort            :: !Word16
      -- ^ Port number to use when translating IP addresses to NodeIds
    , ncoPolicies        :: !(Maybe FilePath)
      -- DOCUMENT THIS FIELD
    , ncoBindAddress     :: !(Maybe NetworkAddress)
      -- ^ A node may have a bind address which differs from its external
      -- address.
    , ncoExternalAddress :: !(Maybe NetworkAddress)
      -- ^ A node must be addressable on the network.
    , ncoCheckPeerHost   :: !Bool
      -- ^ Whether to perform the peer host address consistency check.
      -- The check is necessary to avoid easy denial-of-service attacks,
      -- but can be restrictive in certain scenarios.
    } deriving (Show)

----------------------------------------------------------------------------
-- Parser
----------------------------------------------------------------------------

networkConfigOption :: Opt.Parser NetworkConfigOpts
networkConfigOption = do
    ncoTopology <-
        optional $ Opt.strOption $
        mconcat
            [ Opt.long "topology"
            , Opt.metavar "FILEPATH"
            , Opt.help "Path to a YAML file containing the network topology"
            ]
    ncoSelf <-
        optional $ Opt.option (fromString <$> Opt.str) $
        mconcat
            [ Opt.long "node-id"
            , Opt.metavar "NODE_ID"
            , Opt.help "Identifier for this node within the network"
            ]
    ncoPort <-
        Opt.option Opt.auto $
        mconcat
            [ Opt.long "default-port"
            , Opt.metavar "PORT"
            , Opt.help "Port number for IP address to node ID translation"
            , Opt.value 3000
            ]
    ncoPolicies <-
        Opt.optional $ Opt.strOption $
        mconcat
            [ Opt.long "policies"
            , Opt.metavar "FILEPATH"
            , Opt.help "Path to a YAML file containing the network policies"
            ]
    ncoCheckPeerHost <- (not <$>) .
        Opt.switch $
        mconcat
            [ Opt.long "disable-peer-host-check"
            , Opt.help "DANGER: disable the peer host address consistency check. Makes your node vulnerable"
            ]
    ncoExternalAddress <- optional $ externalNetworkAddressOption Nothing
    ncoBindAddress <- optional $ listenNetworkAddressOption Nothing
    pure $ NetworkConfigOpts {..}

externalNetworkAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
externalNetworkAddressOption na =
    Opt.option (fromParsec addrParserNoWildcard) $
            Opt.long "address"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "IP and port of external address. "
        <> "Please make sure these IP and port (on which node is running) are accessible "
        <> "otherwise proper work of CSL isn't guaranteed. "
        <> "0.0.0.0 is not accepted as a valid host."

listenNetworkAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
listenNetworkAddressOption na =
    Opt.option (fromParsec addrParser) $
            Opt.long "listen"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "IP and port on which to bind and listen. Please make sure these IP "
        <> "and port are accessible, otherwise proper work of CSL isn't guaranteed."

----------------------------------------------------------------------------
-- Defaults
----------------------------------------------------------------------------

-- | The topology we assume when no topology file is specified
defaultTopology :: Y.Topology
defaultTopology =
    Y.TopologyBehindNAT 1 1 defaultDnsDomains

-- | The default DNS domains used for relay discovery
--
-- TODO: Give this a proper value
defaultDnsDomains :: DnsDomains DNS.Domain
defaultDnsDomains = DnsDomains [
      [NodeAddrDNS "todo.defaultDnsDomain.com" Nothing]
    ]

----------------------------------------------------------------------------
-- Monitor for static peers
----------------------------------------------------------------------------

data MonitorEvent
    = MonitorRegister (Peers NodeId -> IO ())
    | MonitorSIGHUP

-- | Monitor for changes to the static config
monitorStaticConfig ::
       Trace IO (Severity, Text)
    -> NetworkConfigOpts
    -> NodeMetadata -- ^ Original metadata (at startup)
    -> Peers NodeId -- ^ Initial value
    -> IO T.StaticPeers
monitorStaticConfig logTrace cfg@NetworkConfigOpts{..} origMetadata initPeers = do
    events :: Chan MonitorEvent <- newChan

#ifdef POSIX
    installHandler SigHUP $ writeChan events MonitorSIGHUP
#endif

    return T.StaticPeers {
        T.staticPeersOnChange = writeChan events . MonitorRegister
      , T.staticPeersMonitoring = loop events initPeers []
      }
  where
    loop :: Chan MonitorEvent
         -> Peers NodeId
         -> [Peers NodeId -> IO ()]
         -> IO ()
    loop events peers handlers = readChan events >>= \case
        MonitorRegister handler -> do
            runHandler peers handler -- Call new handler with current value
            loop events peers (handler:handlers)
        MonitorSIGHUP -> do
            let fp = fromJust ncoTopology
            mParsedTopology <- try $ readTopology fp
            case mParsedTopology of
              Right (Y.TopologyStatic allPeers) -> do
                (newMetadata, newPeers) <-
                    fromPovOf cfg allPeers

                unless (nmType newMetadata == nmType origMetadata) $
                    traceWith logTrace (Error, changedType fp)
                unless (nmMaxSubscrs newMetadata == nmMaxSubscrs origMetadata) $
                    traceWith logTrace (Error, changedMaxSubscrs fp)

                mapM_ (runHandler newPeers) handlers
                traceWith logTrace (Notice, sformat "SIGHUP: Re-read topology")
                loop events newPeers handlers
              Right _otherTopology -> do
                traceWith logTrace (Error, changedFormat fp)
                loop events peers handlers
              Left ex -> do
                traceWith logTrace (Error, readFailed fp ex)
                loop events peers handlers

    runHandler :: forall t . t -> (t -> IO ()) -> IO ()
    runHandler it handler = do
        mu <- try (handler it)
        case mu of
          Left  ex -> traceWith logTrace (Error, handlerError ex)
          Right () -> return ()

    changedFormat, changedType :: FilePath -> Text
    changedFormat     = sformat $ "SIGHUP ("%shown%"): Cannot dynamically change topology."
    changedType       = sformat $ "SIGHUP ("%shown%"): Cannot dynamically change own node type."
    changedMaxSubscrs = sformat $ "SIGHUP ("%shown%"): Cannot dynamically change maximum number of subscribers."

    readFailed :: FilePath -> SomeException -> Text
    readFailed = sformat $ "SIGHUP: Failed to read " % shown % ": " % shown % ". Ignored."

    handlerError :: SomeException -> Text
    handlerError = sformat $
        "Exception thrown by staticPeersOnChange handler: " % shown % ". Ignored."

launchStaticConfigMonitoring ::
       (MonadIO m) => T.Topology k -> m ()
launchStaticConfigMonitoring topology = liftIO action
  where
    action =
        case topology of
            T.TopologyCore {topologyStaticPeers = T.StaticPeers {..}} ->
                staticPeersMonitoring
            T.TopologyRelay {topologyStaticPeers = T.StaticPeers {..}} ->
                staticPeersMonitoring
            _ -> pass

----------------------------------------------------------------------------
-- Interpreter
----------------------------------------------------------------------------

-- | Interpreter for the network config opts
intNetworkConfigOpts
    :: Trace IO (Severity, Text)
    -> NetworkConfigOpts
    -> IO (T.NetworkConfig ())
intNetworkConfigOpts logTrace cfg@NetworkConfigOpts{..} = do
    parsedTopology <-
        case ncoTopology of
            Nothing -> pure defaultTopology
            Just fp -> readTopology fp
    (ourTopology, tcpAddr) <- case parsedTopology of
        Y.TopologyStatic topologyAllPeers -> do
            (md@(NodeMetadata
                     nmType
                     _
                     _
                     nmSubscribe
                     nmValency
                     nmFallbacks
                     _
                     _
                     nmMaxSubscrs), initPeers) <-
                fromPovOf cfg topologyAllPeers
            topologyStaticPeers <- monitorStaticConfig logTrace cfg md initPeers
            topology <- case nmType of
                T.NodeCore  -> return T.TopologyCore{..}
                T.NodeRelay -> return T.TopologyRelay {
                                 topologyStaticPeers,
                                 topologyDnsDomains = nmSubscribe,
                                 topologyValency    = nmValency,
                                 topologyFallbacks  = nmFallbacks,
                                 topologyMaxSubscrs = nmMaxSubscrs
                               }
                T.NodeEdge  -> throwIO NetworkConfigSelfEdge
            tcpAddr <- createTcpAddr
            pure (topology, tcpAddr)
        Y.TopologyBehindNAT
              topologyValency
              topologyFallbacks
              topologyDnsDomains -> do
          -- Behind-NAT topology claims no address for the transport, and also
          -- throws an exception if the --listen parameter is given, to avoid
          -- confusion: if a user gives a --listen parameter then they probably
          -- think the program will bind a socket.
          when (isJust ncoBindAddress) $ throwIO $ RedundantCliParameter $
              "BehindNAT topology is used, no bind address is expected"
          when (isJust ncoExternalAddress) $ throwIO $ RedundantCliParameter $
              "BehindNAT topology is used, no external address is expected"
          pure (T.TopologyBehindNAT{..}, TCP.Unaddressable)
        Y.TopologyP2P
              topologyValency
              topologyFallbacks
              topologyMaxSubscrs -> do
          tcpAddr <- createTcpAddr
          pure ( T.TopologyP2P{..}
               , tcpAddr )
        Y.TopologyTraditional
              topologyValency
              topologyFallbacks
              topologyMaxSubscrs -> do
              tcpAddr <- createTcpAddr
              pure ( T.TopologyTraditional{..}
                   , tcpAddr )

    (enqueuePolicy, dequeuePolicy, failurePolicy) <- case ncoPolicies of
        -- If no policy file is given we just use the default derived from the
        -- topology.
        Nothing -> return
            ( T.topologyEnqueuePolicy ourTopology
            , T.topologyDequeuePolicy ourTopology
            , T.topologyFailurePolicy ourTopology
            )
        -- A policy file is given: the topology-derived defaults are ignored
        -- and we take the complete policy description from the file.
        Just fp -> Y.fromStaticPolicies <$> readPolicies fp

    let networkConfig = T.NetworkConfig
            { ncTopology      = ourTopology
            , ncDefaultPort   = ncoPort
            , ncSelfName      = ncoSelf
            , ncEnqueuePolicy = enqueuePolicy
            , ncDequeuePolicy = dequeuePolicy
            , ncFailurePolicy = failurePolicy
            , ncTcpAddr       = tcpAddr
            , ncCheckPeerHost = ncoCheckPeerHost
            }

    pure networkConfig
  where
    -- Creates transport params out of config.
    createTcpAddr :: IO TCP.TCPAddr
    createTcpAddr = do
        bindAddr@(bindHost, bindPort) <-
            maybe (throwIO MissingBindAddress) pure ncoBindAddress
        let (externalHost, externalPort) = fromMaybe bindAddr ncoExternalAddress
        let tcpHost = BS.C8.unpack bindHost
            tcpPort = show bindPort
            tcpMkExternal = const (BS.C8.unpack externalHost, show externalPort)
        pure $ TCP.Addressable $ TCP.TCPAddrInfo tcpHost tcpPort tcpMkExternal

-- | Perspective on 'AllStaticallyKnownPeers' from the point of view of
-- a single node.
--
-- First component is this node's metadata.
-- Second component is the set of all known peers (routes included).
fromPovOf :: NetworkConfigOpts
          -> Y.AllStaticallyKnownPeers
          -> IO (NodeMetadata, Peers NodeId)
fromPovOf cfg@NetworkConfigOpts{..} allPeers = case ncoSelf of
    Nothing   -> throwIO NetworkConfigSelfUnknown
    Just self -> T.initDnsOnUse $ \resolve -> do
        selfMetadata <- metadataFor allPeers self
        resolved     <- resolvePeers resolve (Y.allStaticallyKnownPeers allPeers)
        routes       <- mkRoutes (second addressToNodeId <$> resolved) (Y.nmRoutes selfMetadata)
        let directory     = M.fromList (map (\(a, b) -> (addressToNodeId b, a)) (M.elems resolved))
        pure (selfMetadata, peersFromList directory routes)
  where

    -- Use the name/metadata association to come up with types and
    -- addresses for each name.
    resolvePeers :: T.Resolver
                 -> Map NodeName Y.NodeMetadata
                 -> IO (Map NodeName (T.NodeType, NetworkAddress))
    resolvePeers resolve = M.traverseWithKey (resolvePeer resolve)

    resolvePeer :: T.Resolver -> NodeName -> Y.NodeMetadata -> IO (T.NodeType, NetworkAddress)
    resolvePeer resolve name metadata =
        (typ,) <$> resolveNodeAddr cfg resolve (name, addr)
      where
        typ  = nmType metadata
        addr = nmAddress metadata

    -- Given a NodeName directory (see 'resolvePeers'), fill in the NodeRoutes
    -- by looking up the relevant names.
    -- It's assumed that each name in a list of alternatives has the same
    -- type.
    mkRoutes :: Map NodeName (T.NodeType, NodeId) -> Y.NodeRoutes -> IO [(T.NodeType, Alts NodeId)]
    mkRoutes directory (Y.NodeRoutes routes) = mapM (mkAlts directory) routes

    mkAlts :: Map NodeName (T.NodeType, NodeId) -> Alts NodeName -> IO (T.NodeType, Alts NodeId)
    mkAlts _ [] = throwIO $ EmptyListOfAltsFor (fromJust ncoSelf)
    mkAlts directory names@(name:_) = do
      -- Use the type associated to the first name, and assume all alts have
      -- same type.
      -- TODO we could easily check that using
      --
      --   mapM (resolveName directory) names :: IO (NodeType, NodeId)
      --
      -- and throw an exception if there's a mismatch.
      typ  <- fmap fst . resolveName directory $ name
      nids <- mapM (fmap snd . resolveName directory) names
      return (typ, nids)

    resolveName :: Map NodeName t -> NodeName -> IO t
    resolveName directory name = case M.lookup name directory of
      Nothing -> throwIO $ UndefinedNodeName name
      Just t  -> return t


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
                -> T.Resolver
                -> (NodeName, NodeAddr (Maybe DNS.Domain))
                -> IO NetworkAddress
resolveNodeAddr cfg _ (_, NodeAddrExact addr mPort) = do
    let port = fromMaybe (ncoPort cfg) mPort
    return (encodeUtf8 @String . show $ addr, port)
resolveNodeAddr cfg resolve (name, NodeAddrDNS mHost mPort) = do
    let host = fromMaybe (nameToDomain name)         mHost
        port = fromMaybe (ncoPort cfg) mPort

    mAddrs <- resolve host
    case mAddrs of
      Left err            -> throwIO $ NetworkConfigDnsError host err
      Right []            -> throwIO $ CannotResolve name
      Right addrs@(_:_:_) -> throwIO $ NoUniqueResolution name addrs
      Right [addr]        -> return $ ipv4ToNetworkAddress addr port
  where
    nameToDomain :: NodeName -> DNS.Domain
    nameToDomain (NodeName n) = BS.C8.pack (toString n)

ipv4ToNetworkAddress :: IPv4 -> Word16 -> NetworkAddress
ipv4ToNetworkAddress addr port = (BS.C8.pack (show addr), port)

metadataFor :: Y.AllStaticallyKnownPeers -> NodeName -> IO Y.NodeMetadata
metadataFor (Y.AllStaticallyKnownPeers allPeers) node =
    case M.lookup node allPeers of
        Nothing       -> throwIO $ UndefinedNodeName node
        Just metadata -> return metadata

readTopology :: FilePath -> IO Y.Topology
readTopology fp = Yaml.decodeFileEither fp >>= \case
    Left  err      -> throwIO $ CannotParseNetworkConfig err
    Right topology -> return topology

readPolicies :: FilePath -> IO Y.StaticPolicies
readPolicies fp = Yaml.decodeFileEither fp >>= \case
    Left  err            -> throwIO $ CannotParsePolicies err
    Right staticPolicies -> return staticPolicies

----------------------------------------------------------------------------
-- Errors
----------------------------------------------------------------------------

-- | Something is wrong with the network configuration
--
-- NOTE: Behind NAT nodes are not given an explicit network configuration file,
-- but instead rely on the default topology. These exceptions should never be
-- thrown for behind NAT nodes, as we don't want to prevent the user application
-- from starting up.
data NetworkConfigException =
    -- | We cannot parse the topology .yaml file
    CannotParseNetworkConfig Yaml.ParseException

    -- | Address to bind on is missing in CLI.
  | MissingBindAddress

    -- | Some passed parameters can't be used together.
  | InconsistentParameters Text

    -- | Some CLI parameter is redundant.
  | RedundantCliParameter Text

    -- | A policy description .yaml was specified but couldn't be parsed.
  | CannotParsePolicies Yaml.ParseException

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
