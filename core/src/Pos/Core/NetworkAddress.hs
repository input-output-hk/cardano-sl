module Pos.Core.NetworkAddress
       ( NetworkAddress
       , localhost
       , addrParser
       , addrParserNoWildcard
       ) where

import           Universum

import qualified Data.ByteString.Char8 as BS8
import qualified Serokell.Util.Parse as P
-- We should really be using Megaparsec here instead of Parsec, but that
-- requires 'Serokell.Util.Parse' to be modified to use Parsec and then
-- have that dependency bubble up.
import           Text.Megaparsec (ParsecT)
import qualified Text.Megaparsec as P
import qualified Text.Megaparsec.Char as P


-- | @"127.0.0.1"@.
localhost :: ByteString
localhost = "127.0.0.1"

-- | Full node address.
type NetworkAddress = (ByteString, Word16)

-- | Parsed for network address in format @host:port@.
addrParser :: ParsecT () String m NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port) <* P.eof

-- | Parses an IPv4 NetworkAddress where the host is not 0.0.0.0.
addrParserNoWildcard :: ParsecT () String m NetworkAddress
addrParserNoWildcard = do
    (host, port) <- addrParser
    if host == BS8.pack "0.0.0.0" then empty
    else return (host, port)
