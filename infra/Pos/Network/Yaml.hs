-- | Infrastructure for parsing the .yaml network topology file
module Pos.Network.Yaml (
    Topology(..)
  , AllStaticallyKnownPeers(..)
  , DnsDomains(..)
  , KademliaParams(..)
  , KademliaId(..)
  , KademliaAddress(..)
  , NodeName(..)
  , NodeRegion(..)
  , NodeRoutes(..)
  , NodeAddr(..)
  , NodeMetadata(..)
  ) where

import           Universum
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=), (.!=))
import           Network.Broadcast.OutboundQueue.Types
import           Pos.Util.Config
import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.HashMap.Lazy     as HM
import qualified Data.Map.Strict       as M
import qualified Network.DNS           as DNS

import           Pos.Network.DnsDomains (DnsDomains(..))

-- | Description of the network topology in a Yaml file
--
-- This differs from 'Pos.Network.Types.Topology' because for static nodes this
-- describes the entire network topology (all statically known nodes), not just
-- the topology from the point of view of the current node.
data Topology =
    TopologyStatic !AllStaticallyKnownPeers
  | TopologyBehindNAT !DnsDomains
  | TopologyP2P !Int !Int
  | TopologyTraditional !Int !Int
  deriving (Show)

-- | All statically known peers in the newtork
data AllStaticallyKnownPeers = AllStaticallyKnownPeers {
    allStaticallyKnownPeers :: !(Map NodeName NodeMetadata)
  }
  deriving (Show)

newtype NodeName = NodeName Text
    deriving (Show, Ord, Eq, IsString)

newtype NodeRegion = NodeRegion Text
    deriving (Show, Ord, Eq, IsString)

newtype NodeRoutes = NodeRoutes [[NodeName]]
    deriving (Show)

data NodeAddr =
    -- | We specify the exact address of this node
    --
    -- If port unspecified, use the default.
    NodeAddrExact ByteString (Maybe Word16)

    -- | Do a DNS lookup to find the node's address
    --
    -- If domain unspecified, use the node's name
    -- If port unspecified, use the default.
  | NodeAddrDNS (Maybe DNS.Domain) (Maybe Word16)
  deriving (Show)

data NodeMetadata = NodeMetadata
    { -- | Node type
      nmType :: !NodeType

      -- | Region
    , nmRegion :: !NodeRegion

      -- | Static peers of this node
    , nmRoutes :: !NodeRoutes

      -- | Address for this node
    , nmAddress :: !NodeAddr
    }
    deriving (Show)

-- | Parameters for Kademlia, in case P2P or traditional topology are used.
data KademliaParams = KademliaParams
    { kpId              :: !(Maybe KademliaId)
      -- ^ Kademlia identifier. Optional; one can be generated for you.
    , kpPeers           :: ![KademliaAddress]
      -- ^ Initial Kademlia peers, for joining the network.
    , kpAddress         :: !(Maybe KademliaAddress)
      -- ^ External Kadmelia address.
    , kpBind            :: !KademliaAddress
      -- ^ Address at which to bind the Kademlia socket.
      -- Shouldn't be necessary to have a separate bind and public address.
      -- The Kademlia instance in fact shouldn't even need to know its own
      -- address (should only be used to bind the socket). But due to the way
      -- that responses for FIND_NODES are serialized, Kademlia needs to know
      -- its own external address [TW-153]. The mainline 'kademlia' package
      -- doesn't suffer this problem.
    , kpExplicitInitial :: !Bool
    , kpDumpFile        :: !(Maybe FilePath)
    }
    deriving (Show)

instance FromJSON KademliaParams where
    parseJSON = A.withObject "KademliaParams" $ \obj -> do
        kpId <- obj .:? "identifier"
        kpPeers <- obj .: "peers"
        kpAddress <- obj .:? "externalAddress"
        kpBind <- obj .: "address"
        kpExplicitInitial <- obj .:? "explicitInitial" .!= False
        kpDumpFile <- obj .:? "dumpFile"
        return KademliaParams {..}

instance ToJSON KademliaParams where
    toJSON KademliaParams {..} = A.object [
          "identifier"      .= kpId
        , "peers"           .= kpPeers
        , "externalAddress" .= kpAddress
        , "address"         .= kpBind
        , "explicitInitial" .= kpExplicitInitial
        , "dumpFile"        .= kpDumpFile
        ]

-- | A Kademlia identifier in text representation (probably base64-url encoded).
newtype KademliaId = KademliaId String
    deriving (Show)

instance FromJSON KademliaId where
    parseJSON = fmap KademliaId . parseJSON

instance ToJSON KademliaId where
    toJSON (KademliaId txt) = toJSON txt

data KademliaAddress = KademliaAddress
    { kaHost :: !String
    , kaPort :: !Word16
    }
    deriving (Show)

instance FromJSON KademliaAddress where
    parseJSON = A.withObject "KademliaAddress " $ \obj ->
        KademliaAddress <$> obj .: "host" <*> obj .: "port"

instance ToJSON KademliaAddress where
    toJSON KademliaAddress {..} = A.object [
          "host" .= kaHost
        , "port" .= kaPort
        ]

{-------------------------------------------------------------------------------
  FromJSON instances
-------------------------------------------------------------------------------}

instance FromJSON NodeRegion where
  parseJSON = fmap NodeRegion . parseJSON

instance FromJSON NodeName where
  parseJSON = fmap NodeName . parseJSON

instance FromJSON NodeRoutes where
  parseJSON = fmap NodeRoutes . parseJSON

instance FromJSON NodeType where
  parseJSON = A.withText "NodeType" $ \typ -> do
      case toString typ of
        "core"     -> return NodeCore
        "edge"     -> return NodeEdge
        "relay"    -> return NodeRelay
        _otherwise -> fail $ "Invalid NodeType " ++ show typ

instance FromJSON DnsDomains where
  parseJSON = fmap (DnsDomains . aux) . parseJSON
    where
      aux :: [[String]] -> [[DNS.Domain]]
      aux = map (map BS.C8.pack)

instance FromJSON NodeMetadata where
  parseJSON = A.withObject "NodeMetadata" $ \obj -> do
      nmType    <- obj .: "type"
      nmRegion  <- obj .: "region"
      nmRoutes  <- obj .: "static-routes"
      nmAddress <- extractAddress obj
      return NodeMetadata{..}
    where
      extractAddress :: A.Object -> A.Parser NodeAddr
      extractAddress obj = do
        mAddr <- obj .:? "addr"
        mHost <- obj .:? "host"
        mPort <- obj .:? "port"
        case (mAddr, mHost) of
          (Just addr, Nothing) -> return $ NodeAddrExact (aux addr)      mPort
          (Nothing,  _)        -> return $ NodeAddrDNS   (aux <$> mHost) mPort
          (Just _, Just _)     -> fail "Cannot use both 'addr' and 'host'"

      aux :: String -> DNS.Domain
      aux = BS.C8.pack

instance FromJSON AllStaticallyKnownPeers where
  parseJSON = A.withObject "AllStaticallyKnownPeers" $ \obj ->
      AllStaticallyKnownPeers . M.fromList <$> mapM aux (HM.toList obj)
    where
      aux :: (Text, A.Value) -> A.Parser (NodeName, NodeMetadata)
      aux (name, val) = (NodeName name, ) <$> parseJSON val

instance FromJSON Topology where
  parseJSON = A.withObject "Topology" $ \obj -> do
      mNodes  <- obj .:? "nodes"
      mRelays <- obj .:? "relays"
      mP2p    <- obj .:? "p2p"
      case (mNodes, mRelays, mP2p) of
        (Just nodes, Nothing, Nothing) ->
            TopologyStatic <$> parseJSON nodes
        (Nothing, Just relays, Nothing) ->
            TopologyBehindNAT <$> parseJSON relays
        (Nothing, Nothing, Just p2p) -> flip (A.withObject "P2P") p2p $ \p2pObj -> do
            variantTxt <- obj .: "variant"
            variant <- flip (A.withText "P2P variant") variantTxt $ \txt -> case txt of
                "traditional" -> pure TopologyTraditional
                "normal"      -> pure TopologyP2P
                _             -> fail "P2P variant: expected 'traditional' or 'normal'"
            valency <- obj .:? "valency" .!= 3
            fallbacks <- obj .:? "fallbacks" .!= 1
            pure (variant valency fallbacks)
        _ ->
          fail "Topology: expected exactly one of 'nodes', 'relays', or 'p2p'"

instance IsConfig Topology where
  configPrefix = return Nothing

{-------------------------------------------------------------------------------
  ToJSON instances
-------------------------------------------------------------------------------}

instance ToJSON NodeRegion where
  toJSON (NodeRegion region) = toJSON region

instance ToJSON NodeName where
  toJSON (NodeName name) = toJSON name

instance ToJSON NodeRoutes where
  toJSON (NodeRoutes routes) = toJSON routes

instance ToJSON NodeType where
  toJSON NodeCore  = toJSON ("core"  :: Text)
  toJSON NodeEdge  = toJSON ("edge"  :: Text)
  toJSON NodeRelay = toJSON ("relay" :: Text)

instance ToJSON DnsDomains where
  toJSON DnsDomains{..} = toJSON $ aux dnsDomains
    where
      aux :: [[DNS.Domain]] -> [[String]]
      aux = map (map BS.C8.unpack)

instance ToJSON NodeMetadata where
  toJSON NodeMetadata{..} = A.object $ addAddress nmAddress [
        "type"          .= nmType
      , "region"        .= nmRegion
      , "static-routes" .= nmRoutes
      ]
    where
      addAddress :: NodeAddr -> [A.Pair] -> [A.Pair]
      addAddress (NodeAddrExact addr mPort) = (++) $ concat [
          [ "host" .= aux addr ]
        , [ "port" .= p | Just p <- [mPort] ]
        ]
      addAddress (NodeAddrDNS mHost mPort) = (++) $ concat [
          [ "host" .= aux h | Just h <- [mHost] ]
        , [ "port" .= p     | Just p <- [mPort] ]
        ]

      aux :: DNS.Domain -> String
      aux = BS.C8.unpack

instance ToJSON AllStaticallyKnownPeers where
  toJSON AllStaticallyKnownPeers{..} =
      A.object (map aux $ M.toList allStaticallyKnownPeers)
    where
      aux :: (NodeName, NodeMetadata) -> A.Pair
      aux (NodeName name, info) = name .= info

instance ToJSON Topology where
  toJSON (TopologyStatic    nodes)  = A.object [ "nodes"  .= nodes                   ]
  toJSON (TopologyBehindNAT relays) = A.object [ "relays" .= relays                  ]
  toJSON (TopologyP2P v f)          = A.object
      [ "p2p" .= A.object [
            "variant"   .= ("normal" :: Text)
          , "valency"   .= v
          , "fallbacks" .= f
          ]
      ]
  toJSON (TopologyTraditional v f)  = A.object
      [ "p2p" .= A.object [
            "variant"   .= ("traditional" :: Text)
          , "valency"   .= v
          , "fallbacks" .= f
          ]
      ]
