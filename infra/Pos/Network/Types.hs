module Pos.Network.Types
    ( NetworkConfig (..)
    , emptyNetworkConfig

    , NodeId (..)

    , NodeType (..)
    , MsgType (..)
    , Origin (..)

    , NodeName (..)
    , NodeRegion (..)
    , NodeRoutes (..)
    , NodeMetadata (..)
    ) where

import           Universum
import           Network.Broadcast.OutboundQueue.Types
import           Node.Internal (NodeId (..))

-- | Information about the network in which a node participates.
data NetworkConfig = NetworkConfig
    { ncNodeType       :: !NodeType
    -- ^ The role that this node plays in the network.
    , ncClassification :: !(Map NodeId (NodeType, [[NodeId]]))
    -- ^ Known classifications of peers, along with their routing information.
    }

emptyNetworkConfig :: NodeType -> NetworkConfig
emptyNetworkConfig nt = NetworkConfig
    { ncNodeType       = nt
    , ncClassification = mempty
    }

newtype NodeName = NodeName Text
    deriving (Show, Ord, Eq)

-- NodeType comes from
-- Network.Broadcast.OutboundQueue.Classification
-- but it should be a cardano-sl thing.
--data NodeType = NodeCore | NodeRelay | NodeEdge
--    deriving (Show, Ord, Eq)

newtype NodeRegion = NodeRegion Text
    deriving (Show, Ord, Eq)

newtype NodeRoutes = NodeRoutes [[NodeName]]
    deriving (Show)

data NodeMetadata = NodeMetadata
    { nmType   :: !NodeType
    , nmRegion :: !NodeRegion
    , nmRoutes :: !NodeRoutes
    }
    deriving (Show)
