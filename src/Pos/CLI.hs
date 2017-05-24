{-# LANGUAGE ApplicativeDo #-}

-- | Module for command-line utilites, parsers and convenient handlers.

module Pos.CLI
       ( addrParser
       , attackTypeParser
       , attackTargetParser
       , defaultLoggerConfig
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
       , walletPortOption
       , networkAddressOption
       , externalNetworkAddressOption
       , listenNetworkAddressOption

       , sysStartOption
       , nodeIdOption
       ) where

import           Universum

import           Control.Lens                         (zoom, (?=))
import           Options.Applicative.Builder.Internal (HasMetavar, HasName)
import qualified Options.Applicative.Simple           as Opt
import           Serokell.Util                        (sec)
import           Serokell.Util.OptParse               (fromParsec)
import           System.Wlog                          (LoggerConfig (..),
                                                       Severity (Info, Warning),
                                                       fromScratch, lcTree, ltSeverity,
                                                       parseLoggerConfig, zoomLogger)
import           Text.Parsec                          (try)
import qualified Text.Parsec.Char                     as P
import qualified Text.Parsec.String                   as P

import           Pos.Binary.Core                      ()
import           Pos.Communication                    (NodeId)
import           Pos.Constants                        (isDevelopment, staticSysStart)
import           Pos.Core                             (Address (..), AddressHash,
                                                       Timestamp (..), decodeTextAddress)
import           Pos.Crypto                           (PublicKey)
import           Pos.Security.CLI                     (AttackTarget (..), AttackType (..))
import           Pos.Ssc.SscAlgo                      (SscAlgo (..))
import           Pos.Util                             ()
import           Pos.Util.TimeWarp                    (NetworkAddress, addrParser,
                                                       addrParserNoWildcard,
                                                       addressToNodeId)

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
    token <- some $ P.noneOf " "
    case decodeTextAddress (toText token) of
      Left _  -> fail "Incorrect address"
      Right r -> return $ addrKeyHash r

attackTargetParser :: P.Parser AttackTarget
attackTargetParser = (PubKeyAddressTarget <$> try base58AddrParser) <|>
                     (NetworkAddressTarget <$> addrParser)

-- | Default logger config. Will be used if `--log-config` argument is
-- not passed. Corresponds to next logger config:
--
-- > node:
-- >   severity: Info
-- >   comm:
-- >     severity: Warning
--
defaultLoggerConfig :: LoggerConfig
defaultLoggerConfig = fromScratch $ zoom lcTree $ zoomLogger "node" $ do
    ltSeverity ?= Info
    zoomLogger "comm" $ ltSeverity ?= Warning

-- | Reads logger config from given path. By default return
-- 'defaultLoggerConfig'.
readLoggerConfig :: MonadIO m => Maybe FilePath -> m LoggerConfig
readLoggerConfig = maybe (return defaultLoggerConfig) parseLoggerConfig

----------------------------------------------------------------------------
-- ClI Options
----------------------------------------------------------------------------

data CommonArgs = CommonArgs
    { logConfig          :: !(Maybe FilePath)
    , logPrefix          :: !(Maybe FilePath)
    , sscAlgo            :: !SscAlgo
    , disablePropagation :: !Bool
    , reportServers      :: ![Text]
    , updateServers      :: ![Text]
    -- distributions, only used in dev mode
    , flatDistr          :: !(Maybe (Int, Int))
    , bitcoinDistr       :: !(Maybe (Int, Int))
    , richPoorDistr      :: !(Maybe (Int, Int, Integer, Double))
    , expDistr           :: !Bool
    , sysStart           :: !Timestamp
      -- ^ The system start time.
    } deriving Show

commonArgsParser :: Opt.Parser CommonArgs
commonArgsParser = do
    logConfig <- optionalLogConfig
    logPrefix <- optionalLogPrefix
    --
    sscAlgo <- sscAlgoOption
    --
    disablePropagation <- disablePropagationOption
    --
    reportServers <- reportServersOption
    updateServers <- updateServersOption
    -- distributions
    flatDistr     <- if isDevelopment then flatDistrOptional else pure Nothing
    bitcoinDistr  <- if isDevelopment then btcDistrOptional  else pure Nothing
    richPoorDistr <- if isDevelopment then rnpDistrOptional  else pure Nothing
    expDistr      <- if isDevelopment then expDistrOption    else pure False
    --
    sysStart <- sysStartParser
    pure CommonArgs{..}

sysStartParser :: Opt.Parser Timestamp
sysStartParser = Opt.option (Timestamp . sec <$> Opt.auto) $
    Opt.long    "system-start" <>
    Opt.metavar "TIMESTAMP" <>
    defaultValue
  where
    -- In development mode, this parameter is mandatory.
    -- In production mode, it is optional, and its default value is populated
    -- from `staticSysStart`, which gets it from the config file.
    defaultValue =
        if isDevelopment then mempty else Opt.value staticSysStart

templateParser :: (HasName f, HasMetavar f) => String -> String -> String -> Opt.Mod f a
templateParser long metavar help =
    Opt.long long
    <> Opt.metavar metavar
    <> Opt.help help

networkAddressOption :: String -> String -> Opt.Parser NetworkAddress
networkAddressOption longOption helpMsg =
    Opt.option (fromParsec addrParserNoWildcard) $
        templateParser longOption "HOST:PORT" helpMsg

nodeIdOption :: String -> String -> Opt.Parser NodeId
nodeIdOption longOption helpMsg =
    Opt.option (fromParsec $ addressToNodeId <$> addrParser) $
        templateParser longOption "HOST:PORT" helpMsg

optionalLogConfig :: Opt.Parser (Maybe FilePath)
optionalLogConfig =
    Opt.optional $ Opt.strOption $
        templateParser "log-config" "FILEPATH" "Path to logger configuration"

optionalLogPrefix :: Opt.Parser (Maybe String)
optionalLogPrefix =
    optional $ Opt.strOption $
        templateParser "logs-prefix" "FILEPATH" "Prefix to logger output path"

optionalJSONPath :: Opt.Parser (Maybe FilePath)
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

reportServersOption :: Opt.Parser [Text]
reportServersOption =
    many $
    toText <$>
    Opt.strOption
        (templateParser
             "report-server"
             "URI"
             "Reporting server to send crash/error logs on")

updateServersOption :: Opt.Parser [Text]
updateServersOption =
    many $
    toText <$>
    Opt.strOption
        (templateParser "update-server" "URI" "Server to download updates from")

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

rnpDistrOptional :: Opt.Parser (Maybe (Int, Int, Integer, Double))
rnpDistrOptional =
    Opt.optional $
        Opt.option Opt.auto $
            templateParser
                "rich-poor-distr"
                "(INT,INT,INT,FLOAT)"
                "Use rich'n'poor stake distribution with given parameters\
                \ (number of richmen, number of poors, total stake, richmen's\
                \ share of stake)"

expDistrOption :: Opt.Parser Bool
expDistrOption =
    Opt.switch
        (Opt.long "exp-distr" <> Opt.help "Enable exponential distribution")

timeLordOption :: Opt.Parser Bool
timeLordOption =
    Opt.switch
        (Opt.long "time-lord" <>
         Opt.help "Peer is time lord, i.e. one responsible for system start time decision\
                  \ & propagation (used only in development)")

webPortOption :: Word16 -> String -> Opt.Parser Word16
webPortOption portNum help =
    Opt.option Opt.auto $
        templateParser "web-port" "PORT" help -- "Port for web server"
        <> Opt.value portNum
        <> Opt.showDefault

walletPortOption :: Word16 -> String -> Opt.Parser Word16
walletPortOption portNum help =
    Opt.option Opt.auto $
        templateParser "wallet-port" "PORT" help -- "Port for wallet"
        <> Opt.value portNum
        <> Opt.showDefault

externalNetworkAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
externalNetworkAddressOption na =
    Opt.option (fromParsec addrParserNoWildcard) $
            Opt.long "address"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "Ip and port of external address. "
        <> "Please mind that you need to specify actual accessible "
        <> "ip of host, at which node is run,"
        <> " otherwise work of CSL is not guaranteed. "
        <> "0.0.0.0 is not accepted as a valid host."

listenNetworkAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
listenNetworkAddressOption na =
    Opt.option (fromParsec addrParser) $
            Opt.long "listen"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "Ip and port on which to bind and listen."

sysStartOption :: Opt.Parser Timestamp
sysStartOption = Opt.option (Timestamp . sec <$> Opt.auto) $
    Opt.long    "system-start" <>
    Opt.metavar "TIMESTAMP" <>
    Opt.value   staticSysStart
