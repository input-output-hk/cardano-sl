
-- | Utility functions related to time-warp project.

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

-- | @"127.0.0.1"@.
localhost :: ByteString
localhost = "127.0.0.1"

-- | Full node address.
type NetworkAddress = (ByteString, Word16)

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

-- | Parsed for network address in format @host:port@.
addrParser :: P.Parser NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port) <* P.eof

-- | Parses an IPv4 NetworkAddress where the host is not 0.0.0.0.
addrParserNoWildcard :: P.Parser NetworkAddress
addrParserNoWildcard = do
    (host, port) <- addrParser
    if host == BS8.pack "0.0.0.0" then empty
    else return (host, port)
