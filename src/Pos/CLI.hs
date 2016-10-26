module Pos.CLI (
  dhtKeyParser,
  dhtNodeParser
) where

import           Control.Monad                      (fail)
import           Pos.DHT                            (DHTKey, DHTNode (..), bytesToDHTKey)
import qualified Serokell.Util.Parse                as P
import qualified Text.ParserCombinators.Parsec.Char as P
import           Universum

dhtKeyParser :: P.Parser DHTKey
dhtKeyParser = P.base64Url >>= toDHTKey
  where
    toDHTKey bytes = case bytesToDHTKey bytes of
                       Left e    -> fail e
                       Right key -> return key

dhtNodeParser :: P.Parser DHTNode
dhtNodeParser = (\host port id -> DHTNode (toS host, port) id) <$> P.host <*> (P.char ':' *> P.port) <*> (P.char '/' *> dhtKeyParser)
