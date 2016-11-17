module Pos.CLI
       ( dhtKeyParser
       , addrParser
       , dhtNodeParser
       , sscAlgoParser
       ) where

import           Control.Monad                      (fail)
import           Control.TimeWarp.Rpc               (NetworkAddress)
import           Data.Either                        (either)
import           Pos.DHT.Types                      (DHTKey, DHTNode (..), bytesToDHTKey)
import           Pos.Ssc.SscAlgo                    (SscAlgo (..))
import qualified Serokell.Util.Parse                as P
import qualified Text.ParserCombinators.Parsec.Char as P
import           Universum

dhtKeyParser :: P.Parser DHTKey
dhtKeyParser = P.base64Url >>= toDHTKey
  where
    toDHTKey = either fail return . bytesToDHTKey

addrParser :: P.Parser NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port)

dhtNodeParser :: P.Parser DHTNode
dhtNodeParser = DHTNode <$> addrParser <*> (P.char '/' *> dhtKeyParser)

sscAlgoParser :: P.Parser SscAlgo
sscAlgoParser = GodTossingAlgo <$ (P.string "GodTossing") <|>
                NistBeaconAlgo   <$ (P.string "NistBeacon")
