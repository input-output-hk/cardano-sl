module Pos.Infra.Util.TimeWarp
       ( NetworkAddress
       , localhost
       , addressToNodeId
       , addressToNodeId'
       , nodeIdToAddress
       , addrParser
       , addrParserNoWildcard

       ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8
import qualified Network.Transport.TCP.Internal as TCP
import           Node (NodeId (..))

-- Re-export these while refactoing.
import           Pos.Core.NetworkAddress (NetworkAddress, addrParser,
                     addrParserNoWildcard, localhost)

-- TODO: What about node index, i.e. last number in '127.0.0.1:3000:0' ?
addressToNodeId :: NetworkAddress -> NodeId
addressToNodeId = addressToNodeId' 0

addressToNodeId' :: Word32 -> NetworkAddress -> NodeId
addressToNodeId' eId (host, port) =
    NodeId $ TCP.encodeEndPointAddress (BS8.unpack host) (show port) eId

nodeIdToAddress :: NodeId -> Maybe NetworkAddress
nodeIdToAddress (NodeId ep) = do
    (hostName, strPort, _) <- TCP.decodeEndPointAddress ep
    port <- readMaybe strPort
    return (BS8.pack hostName, port)
