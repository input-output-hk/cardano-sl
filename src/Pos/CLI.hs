{-# LANGUAGE CPP             #-}
{-# LANGUAGE OverloadedLists #-}

-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.CLI
       ( addrParser
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
       , dhtKeyParser
       , dhtNodeParser
       , readLoggerConfig
       , sscAlgoParser

       -- | CLI options and flags
       , CommonArgs (..)
       , commonArgsParser
       , optionalJSONPath
       , optionalLogPrefix
       , portOption
       , timeLordOption
       , webPortOption
       ) where

import           Universum

import           Data.Default                         (def)
import           Data.Either                          (either)
import           Data.Text                            (pack)
import           Options.Applicative.Builder.Internal (HasMetavar, HasName)
import qualified Options.Applicative.Simple           as Opt (Mod, Parser, auto, help,
                                                              long, metavar, option,
                                                              optional, showDefault,
                                                              strOption, switch, value)

import           Pos.Binary.Address                   ()
import           Pos.Crypto                           (PublicKey)
import           Pos.DHT.Model.Types                  (DHTKey, DHTNode (..),
                                                       bytesToDHTKey)
import           Pos.Security.Types                   (AttackTarget (..), AttackType (..))
import           Pos.Ssc.SscAlgo                      (SscAlgo (..))
import           Pos.Types.Address                    (Address (..), AddressHash,
                                                       decodeTextAddress)
import           Pos.Util.TimeWarp                    (NetworkAddress)
import           Serokell.Util.OptParse               (fromParsec)
import qualified Serokell.Util.Parse                  as P
import           System.Wlog                          (LoggerConfig (..),
                                                       Severity (Info, Warning),
                                                       parseLoggerConfig)
import           Text.ParserCombinators.Parsec        (many1, try)
import qualified Text.ParserCombinators.Parsec.Char   as P

-- | Parser for DHT key.
dhtKeyParser :: P.Parser DHTKey
dhtKeyParser = P.base64Url >>= toDHTKey
  where
    toDHTKey = either fail return . bytesToDHTKey

-- | Parsed for network address in format @host:port@.
addrParser :: P.Parser NetworkAddress
addrParser = (,) <$> (encodeUtf8 <$> P.host) <*> (P.char ':' *> P.port)

-- | Parser for 'DHTNode'.
dhtNodeParser :: P.Parser DHTNode
dhtNodeParser = DHTNode <$> addrParser <*> (P.char '/' *> dhtKeyParser)

-- | Decides which secret-sharing algorithm to use.
sscAlgoParser :: P.Parser SscAlgo
sscAlgoParser = GodTossingAlgo <$ (P.string "GodTossing") <|>
                NistBeaconAlgo   <$ (P.string "NistBeacon")

attackTypeParser :: P.Parser AttackType
attackTypeParser = P.string "No" >>
    AttackNoBlocks <$ (P.string "Blocks") <|>
    AttackNoCommitments <$ (P.string "Commitments")

base58AddrParser :: P.Parser (AddressHash PublicKey)
base58AddrParser = do
    token <- many1 $ P.noneOf " "
    case decodeTextAddress $ pack token of
      Left _  -> fail "Incorrect address"
      Right r -> return $ addrKeyHash r

attackTargetParser :: P.Parser AttackTarget
attackTargetParser = (PubKeyAddressTarget <$> try base58AddrParser) <|>
                     (NetworkAddressTarget <$> addrParser)

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

------------------------------------------------------------------------------------------
-- CLI Options
------------------------------------------------------------------------------------------

data CommonArgs = CommonArgs
    { dhtExplicitInitial :: !Bool
    , dhtPeers           :: ![DHTNode]
    , logConfig          :: !(Maybe FilePath)
    , logPrefix          :: !(Maybe FilePath)
    , sscAlgo            :: !SscAlgo
    , disablePropagation :: !Bool
#ifdef DEV_MODE
    , flatDistr          :: !(Maybe (Int, Int))
    , bitcoinDistr       :: !(Maybe (Int, Int))
#endif
    } deriving Show

commonArgsParser :: [Char] -> Opt.Parser CommonArgs
commonArgsParser peerHelpMsg = CommonArgs
    <$> explicitInitial
    <*> many (peerOption peerHelpMsg)
    <*> optionalLogConfig
    <*> optionalLogPrefix
    <*> sscAlgoOption
    <*> disablePropagationOption
#ifdef DEV_MODE
    <*> flatDistrOptional
    <*> btcDistrOptional
#endif

templateParser :: (HasName f, HasMetavar f) => [Char] -> [Char] -> [Char] -> Opt.Mod f a
templateParser long metavar help =
    Opt.long long
    <> Opt.metavar metavar
    <> Opt.help help

explicitInitial :: Opt.Parser Bool
explicitInitial =
    Opt.switch
        (Opt.long "explicit-initial" <>
         Opt.help "Explicitely contact to initial peers as to neighbors (even if they\
                  \ appeared offline once)")

peerOption :: [Char] -> Opt.Parser DHTNode
peerOption peerHelpMsg =
    Opt.option (fromParsec dhtNodeParser) $
        templateParser "peer" "HOST:PORT/HOST_ID" peerHelpMsg

optionalLogConfig :: Opt.Parser (Maybe [Char])
optionalLogConfig =
    Opt.optional $ Opt.strOption $
        templateParser "log-config" "FILEPATH" "Path to logger configuration"

optionalLogPrefix :: Opt.Parser (Maybe [Char])
optionalLogPrefix =
    optional $ Opt.strOption $
        templateParser "logs-prefix" "FILEPATH" "Prefix to logger output path"

optionalJSONPath :: Opt.Parser (Maybe [Char])
optionalJSONPath =
    Opt.optional $ Opt.strOption $
        templateParser "json-log" "FILEPATH" "Path to json log file"

portOption :: Word16 -> Opt.Parser Word16
portOption portNum =
    Opt.option Opt.auto $
        templateParser "port" "PORT" "Port to work on"
        <> Opt.value portNum
        <> Opt.showDefault

sscAlgoOption :: Opt.Parser SscAlgo
sscAlgoOption =
    Opt.option (fromParsec sscAlgoParser) $
        templateParser "ssc-algo"
                       "ALGO"
                       "Shared Seed Calculation algorithm which nodes will use"
        <> Opt.value GodTossingAlgo
        <> Opt.showDefault

disablePropagationOption :: Opt.Parser Bool
disablePropagationOption =
    Opt.switch
        (Opt.long "disable-propagation" <>
         Opt.help "Disable network propagation (transactions, SSC data, blocks). I.e.\
                  \ all data is to be sent only by entity who creates data and entity is\
                  \ yosend it to all peers on his own")

#ifdef DEV_MODE
flatDistrOptional :: Opt.Parser (Maybe (Int, Int))
flatDistrOptional =
    Opt.optional $
        Opt.option Opt.auto $
            templateParser
                "flat-distr"
                "(INT,INT)"
                "Use flat stake distribution with given parameters (nodes, coins)"

btcDistrOptional :: Opt.Parser (Maybe (Int, Int))
btcDistrOptional =
    Opt.optional $
        Opt.option Opt.auto $
            templateParser
                "bitcoin-distr"
                "(INT,INT)"
                "Use bitcoin stake distribution with given parameters (nodes,\
                \ coins)"
#endif

timeLordOption :: Opt.Parser Bool
timeLordOption =
    Opt.switch
        (Opt.long "time-lord" <>
         Opt.help "Peer is time lord, i.e. one responsible for system start time decision\
                  \ & propagation (used only in development)")

webPortOption :: Word16 -> [Char] -> Opt.Parser Word16
webPortOption portNum help =
    Opt.option Opt.auto $
        templateParser "web-port" "PORT" help -- "Port for web server"
        <> Opt.value portNum
        <> Opt.showDefault
