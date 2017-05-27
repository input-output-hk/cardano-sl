
-- | Common things used in `Pos.Crypto.Arbitrary` and `Pos.Util.Arbitrary`

module Pos.Util.TimeWarp
       ( NetworkAddress
       , localhost

       , currentTime
       , addressToNodeId
       , addressToNodeId'
       , nodeIdToAddress
       , addrParser
       , readAddrFile
       , addrParserNoWildcard
       ) where

import qualified Data.ByteString.Char8          as BS8
import           Data.Time.Units                (Microsecond)
import           Formatting                     (build, formatToString, shown, (%))
import           Mockable                       (realTime)
import qualified Network.Transport.TCP.Internal as TCP
import           Node                           (NodeId (..))
import qualified Serokell.Util.Parse            as P
import           Text.Parsec                    as P
import qualified Text.Parsec.String             as P
import           Universum

-- | @"127.0.0.1"@.
localhost :: ByteString
localhost = "127.0.0.1"

-- | Full node address.
type NetworkAddress = (ByteString, Word16)

-- | Temporal solution
currentTime :: MonadIO m => m Microsecond
currentTime = realTime

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

readAddrFile :: FilePath -> IO [NetworkAddress]
readAddrFile path = do
    xs <- lines <$> readFile path
    let parseLine x = case P.parse (addrParser <* P.eof) "" (toString x) of
            Left err -> fail $ formatToString
                ("error when parsing network address "%shown%
                 " from file "%build%": "%shown) x path err
            Right a -> pure a
    mapM parseLine xs

-- | Parses an IPv4 NetworkAddress where the host is not 0.0.0.0.
addrParserNoWildcard :: P.Parser NetworkAddress
addrParserNoWildcard = do
    (host, port) <- addrParser
    if host == BS8.pack "0.0.0.0" then empty
    else return (host, port)
