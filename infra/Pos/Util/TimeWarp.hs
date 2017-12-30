
-- | Utility functions related to time-warp project.
-- time-warp provides a reliable networking layer with testing layers built in
-- originally developed under the project https://github.com/serokell/time-warp-nt and wasthen merged into cardano-sl
-- EndPoint refers to a logical endpoint within a transport instance, a thin abstraction over the TCP/IP notion of an endpoint, addressed via a hostname and port, are inary strings with the structure HOST:PORT:LOCAL_ID, for example, 192.168.0.1:3010:0.
-- Note that while a transport instance listens on a single port,
-- in principle there can be multiple addressable endpoints within a single transport instance,
-- and this is what the LOCAL_ID refers to.
-- Cardano SL, however, does not currently make use of this feature, so it always uses LOCAL_ID 0.
-- The 'NodeID' wraps a network-transport endpoint address, see `networking/src/Node/Internal.hs`
-- for example Node ID could be defined like `NodeId (TCP.encodeEndPointAddress "127.0.0.1" serverPort 0)`
-- See https://cardanodocs.com/technical/protocols/network-transport/

module Pos.Util.TimeWarp
       ( NetworkAddress
       , localhost
       , addressToNodeId
       , addressToNodeId'
       , nodeIdToAddress
       , addrParser
       , addrParserNoWildcard
       , module JsonLog
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8
import           JsonLog
import qualified Network.Transport.TCP.Internal as TCP
import           Node (NodeId (..))
import qualified Serokell.Util.Parse as P
import qualified Text.Parsec as P
import qualified Text.Parsec.Text as P

-- | 'localhost' as a network address @"127.0.0.1"@.
localhost :: ByteString
localhost = "127.0.0.1"

-- | The Type 'NetworkAddress' is Full node address the IP and the port minus the LOCAL_ID
type NetworkAddress = (ByteString, Word16)

-- | The Function 'addressToNodeId' converts a network address into a 'NodeId'
-- The LOCAL_ID, i.e. last number in '127.0.0.1:3000:0' is always 0
-- TODO : Possibly support multiple local IDS.
addressToNodeId :: NetworkAddress -> NodeId
addressToNodeId = addressToNodeId' 0

-- | The Function 'addressToNodeId\'' returns a NodeId with a host, port and LOCAL_ID(eId)
addressToNodeId' :: Word32 -> NetworkAddress -> NodeId
addressToNodeId' eId (host, port) =
    NodeId $ TCP.encodeEndPointAddress (BS8.unpack host) (show port) eId

-- | The Function 'nodeIdToAddress' Converts a NodeId (Endpoint) into a NetworkAddress (host, port) if possible
nodeIdToAddress :: NodeId -> Maybe NetworkAddress
nodeIdToAddress (NodeId ep) = do
    (hostName, strPort, _) <- TCP.decodeEndPointAddress ep
    port <- readMaybe strPort
    return (BS8.pack hostName, port)

-- | The Function 'addrParser' parses a network address in the format @host:port@ returning (host, port)
addrParser :: P.Parser NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port) <* P.eof

-- | The Function 'addrParserNoWildcard' parses an IPv4 NetworkAddress where the host is not 0.0.0.0 returning (host, port)
-- or empty if the address is a wildcard.
addrParserNoWildcard :: P.Parser NetworkAddress
addrParserNoWildcard = do
    (host, port) <- addrParser
    if host == BS8.pack "0.0.0.0" then empty
    else return (host, port)
