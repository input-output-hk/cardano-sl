module Pos.DHT.Real.Param
       ( KademliaParams(..)
       ) where

import           Pos.DHT.Model.Types (DHTKey)
import           Pos.Util.TimeWarp   (NetworkAddress)
import           Universum

-- | Parameters for the Kademlia DHT subsystem.
data KademliaParams = KademliaParams
    { kpNetworkAddress  :: !NetworkAddress
    , kpPeers           :: ![NetworkAddress] -- ^ Peers passed from CLI
    , kpKey             :: !(Maybe DHTKey)
    , kpExplicitInitial :: !Bool
    , kpDump            :: !FilePath         -- ^ Path to kademlia dump file
    , kpExternalAddress :: !NetworkAddress   -- ^ External address of node
    } deriving (Show)
