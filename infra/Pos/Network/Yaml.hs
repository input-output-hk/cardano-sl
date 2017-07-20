-- | Infrastructure for parsing the .yaml network topology file
module Pos.Network.Yaml (
    Topology(..)
  , AllStaticallyKnownPeers(..)
  , NodeName(..)
  , NodeRegion(..)
  , NodeRoutes(..)
  , NodeMetadata(..)
  ) where

import           Universum
import           Data.Aeson (FromJSON(..), ToJSON(..), (.:), (.:?), (.=))
import qualified Data.Aeson            as A
import qualified Data.Aeson.Types      as A
import qualified Data.ByteString.Char8 as BS.C8
import qualified Data.HashMap.Lazy     as HM
import qualified Data.Map.Strict       as M
import qualified Data.Text             as T
import qualified Network.DNS           as DNS

import           Network.Broadcast.OutboundQueue.Types
import           Pos.Util.Config
import           Pos.Network.DnsDomains (DnsDomains(..))

-- | Description of the network topology in a Yaml file
--
-- This differs from 'Pos.Network.Types.Topology' because for static nodes this
-- describes the entire network topology (all statically known nodes), not just
-- the topology from the point of view of the current node.
data Topology =
    TopologyStatic !AllStaticallyKnownPeers
  | TopologyBehindNAT !DnsDomains
  | TopologyP2P
  | TopologyTransitional
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

data NodeMetadata = NodeMetadata
    { nmType   :: !NodeType
    , nmRegion :: !NodeRegion
    , nmRoutes :: !NodeRoutes
    }
    deriving (Show)

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
      case T.unpack typ of
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
      nmType   <- obj .: "type"
      nmRegion <- obj .: "region"
      nmRoutes <- obj .: "static-routes"
      return NodeMetadata{..}

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
      case (mNodes, mRelays) of
        (Just nodes, Nothing)  -> TopologyStatic    <$> parseJSON nodes
        (Nothing, Just relays) -> TopologyBehindNAT <$> parseJSON relays
        (Just _, Just _) ->
          fail "Topology: expected either 'nodes' or 'relays', not both"
        (Nothing, Nothing) ->
          fail "Topology: expected 'nodes' or 'relays' field"

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
  toJSON NodeMetadata{..} = A.object [
        "type"          .= nmType
      , "region"        .= nmRegion
      , "static-routes" .= nmRoutes
      ]

instance ToJSON AllStaticallyKnownPeers where
  toJSON AllStaticallyKnownPeers{..} =
      A.object (map aux $ M.toList allStaticallyKnownPeers)
    where
      aux :: (NodeName, NodeMetadata) -> A.Pair
      aux (NodeName name, info) = name .= info

instance ToJSON Topology where
  toJSON (TopologyStatic    nodes)  = A.object [ "nodes"  .= nodes ]
  toJSON (TopologyBehindNAT relays) = A.object [ "relays" .= relays ]
  toJSON (TopologyP2P)              = error "TODO: toJSON TopologyP2P"
  toJSON (TopologyTransitional)     = error "TODO: toJSON TopologyTransitional"
