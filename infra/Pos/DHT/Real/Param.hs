module Pos.DHT.Real.Param
       ( KademliaParams(..)
       ) where

import           Universum
import           Pos.Util.TimeWarp   (NetworkAddress)
import           Pos.DHT.Model.Types (DHTKey, DHTNode)

-- | Parameters for the Kademlia DHT subsystem.
data KademliaParams = KademliaParams {
      kpIpPort          :: !NetworkAddress
    , kpPeers           :: ![DHTNode]      -- ^ Peers passed from CLI
    , kpKey             :: !(Maybe DHTKey)
    , kpExplicitInitial :: !Bool
    , kpDump            :: !FilePath       -- ^ Path to kademlia dump file
    }

