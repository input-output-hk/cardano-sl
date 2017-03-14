{-# LANGUAGE ApplicativeDo #-}

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
       , walletPortOption
       , ipPortOption

       , readPeersFile
       ) where

import           Control.Lens                         (zoom, (?=))
import qualified Data.Text                            as T
import           Formatting                           (build, formatToString, shown, (%))
import           Options.Applicative.Builder.Internal (HasMetavar, HasName)
import qualified Options.Applicative.Simple           as Opt
import           Serokell.Util.OptParse               (fromParsec)
import           System.Wlog                          (LoggerConfig (..),
                                                       Severity (Info, Warning),
                                                       fromScratch, lcTree, ltSeverity,
                                                       parseLoggerConfig, zoomLogger)
import           Text.Parsec                          (eof, parse, try)
import qualified Text.Parsec.Char                     as P
import qualified Text.Parsec.String                   as P
import           Universum

import           Pos.Binary.Core                      ()
import           Pos.Constants                        (isDevelopment)
import           Pos.Core                             (Address (..), AddressHash,
                                                       decodeTextAddress)
import           Pos.Crypto                           (PublicKey)
import           Pos.DHT.Model.Types                  (DHTNode (..), dhtKeyParser,
                                                       dhtNodeParser)
import           Pos.Security.CLI                     (AttackTarget (..), AttackType (..))
import           Pos.Ssc.SscAlgo                      (SscAlgo (..))
import           Pos.Util                             ()
import           Pos.Util.TimeWarp                    (NetworkAddress, addrParser)

-- | Parse 'DHTNode's from a file (nodes should be separated by newlines).
readPeersFile :: FilePath -> IO [DHTNode]
readPeersFile path = do
    xs <- lines <$> readFile path
    let parseLine x = case parse (dhtNodeParser <* eof) "" (toString x) of
            Left err -> fail $ formatToString
                ("error when parsing peer "%shown%
                 " from peers file "%build%": "%shown) x path err
            Right a -> return a
    mapM parseLine xs

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
    { dhtExplicitInitial :: !Bool
    , dhtPeers           :: ![DHTNode]
    , dhtPeersFile       :: !(Maybe FilePath)
    , logConfig          :: !(Maybe FilePath)
    , logPrefix          :: !(Maybe FilePath)
    , sscAlgo            :: !SscAlgo
    , disablePropagation :: !Bool
    , reportServers      :: ![Text]
    , updateServers      :: ![Text]
    -- distributions, only used in dev mode
    , flatDistr          :: !(Maybe (Int, Int))
    , bitcoinDistr       :: !(Maybe (Int, Int))
    , expDistr           :: !Bool
    } deriving Show

commonArgsParser :: String -> Opt.Parser CommonArgs
commonArgsParser peerHelpMsg = do
    dhtExplicitInitial <- explicitInitial
    --
    dhtPeers     <- many (peerOption peerHelpMsg)
    dhtPeersFile <- optionalPeersFile
    --
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
    flatDistr    <- if isDevelopment then flatDistrOptional else pure Nothing
    bitcoinDistr <- if isDevelopment then btcDistrOptional  else pure Nothing
    expDistr     <- if isDevelopment then expDistrOption    else pure False
    --
    pure CommonArgs{..}

templateParser :: (HasName f, HasMetavar f) => String -> String -> String -> Opt.Mod f a
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

peerOption :: String -> Opt.Parser DHTNode
peerOption peerHelpMsg =
    Opt.option (fromParsec dhtNodeParser) $
        templateParser "peer" "HOST:PORT/HOST_ID" peerHelpMsg

optionalPeersFile :: Opt.Parser (Maybe FilePath)
optionalPeersFile =
    Opt.optional $ Opt.strOption $
        templateParser "peers-file" "FILEPATH" "Path to peer list"

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
    T.pack <$>
    Opt.strOption
        (templateParser
             "report-server"
             "URI"
             "Reporting server to send crash/error logs on")

updateServersOption :: Opt.Parser [Text]
updateServersOption =
    many $
    T.pack <$>
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

ipPortOption :: NetworkAddress -> Opt.Parser NetworkAddress
ipPortOption na =
    Opt.option (fromParsec addrParser) $
            Opt.long "listen"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> Opt.value na
  where
    helpMsg = "Ip and port on which to listen. "
        <> "Please mind that you need to specify actual accessible "
        <> "ip of host, at which node is run,"
        <> " otherwise work of CSL is not guaranteed."
