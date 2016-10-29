module Pos.CLI (
  dhtKeyParser,
  dhtNodeParser
) where

import           Control.Monad                      (fail)
import           Data.Either                        (either)
import           Pos.DHT                            (DHTKey, DHTNode (..), bytesToDHTKey)
import qualified Serokell.Util.Parse                as P
import qualified Text.ParserCombinators.Parsec.Char as P
import           Universum

dhtKeyParser :: P.Parser DHTKey
dhtKeyParser = P.base64Url >>= toDHTKey
  where
    toDHTKey = either fail return . bytesToDHTKey

dhtNodeParser :: P.Parser DHTNode
dhtNodeParser = (\host port id -> DHTNode (toS host, port) id) <$> P.host <*> (P.char ':' *> P.port) <*> (P.char '/' *> dhtKeyParser)
