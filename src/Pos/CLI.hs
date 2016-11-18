{-# LANGUAGE OverloadedLists #-}

-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.CLI
       ( addrParser
       , dhtKeyParser
       , dhtNodeParser
       , readLoggerConfig
       , sscAlgoParser
       ) where

import           Universum

import           Control.Monad                      (fail)
import           Control.TimeWarp.Rpc               (NetworkAddress)
import           Data.Default                       (def)
import           Data.Either                        (either)
import           Pos.DHT.Types                      (DHTKey, DHTNode (..), bytesToDHTKey)
import           Pos.Ssc.SscAlgo                    (SscAlgo (..))
import qualified Serokell.Util.Parse                as P
import           System.Wlog                        (LoggerConfig (..),
                                                     Severity (Info, Warning),
                                                     parseLoggerConfig)
import qualified Text.ParserCombinators.Parsec.Char as P

dhtKeyParser :: P.Parser DHTKey
dhtKeyParser = P.base64Url >>= toDHTKey
  where
    toDHTKey = either fail return . bytesToDHTKey

addrParser :: P.Parser NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port)

dhtNodeParser :: P.Parser DHTNode
dhtNodeParser = DHTNode <$> addrParser <*> (P.char '/' *> dhtKeyParser)

sscAlgoParser :: P.Parser SscAlgo
sscAlgoParser = DynamicStateAlgo <$ (P.string "DynamicState") <|>
                NistBeaconAlgo   <$ (P.string "NistBeacon")

-- | Default logger config. Will be used if `--log-config` argument is not passed.
-- Corresponds to next logger config:
--
-- > node:
-- >   severity: Info
-- >   comm:
-- >     severity: Warning
--
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = def { lcSubloggers = defSubloggers }
  where
    defSubloggers = [ ( "node"
                      , def
                        { lcSeverity = Just Info
                        , lcSubloggers = [ ( "comm"
                                           , def { lcSeverity = Just Warning }
                                           )
                                         ]
                        }
                      )
                    ]

-- | Reads logger config from given path. By default return 'defaultLoggerConfig'.
readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
readLoggerConfig = maybe (return defaultLoggerConfig) parseLoggerConfig
