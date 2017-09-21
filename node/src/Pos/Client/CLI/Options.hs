{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}

-- | Module for command-line options and flags

module Pos.Client.CLI.Options
       ( CommonArgs (..)
       , commonArgsParser
       , configurationOptionsParser
       , optionalJSONPath
       , optionalLogPrefix
       , portOption
       , timeLordOption
       , webPortOption
       , walletAddressOption
       , networkAddressOption
       , externalNetworkAddressOption
       , listenNetworkAddressOption
       , templateParser
       , sscAlgoOption

       , nodeIdOption

       ) where

import           Universum

import           Data.Default                         (def)
import qualified Options.Applicative                  as Opt
import           Options.Applicative.Builder.Internal (HasMetavar, HasName)
import           Serokell.Util                        (sec)
import           Serokell.Util.OptParse               (fromParsec)

import           Pos.Binary.Core                      ()
import           Pos.Client.CLI.Util                  (sscAlgoParser)
import           Pos.Communication                    (NodeId)
import           Pos.Core                             (Timestamp (..))
import           Pos.Launcher.Configuration           (ConfigurationOptions (..))
import           Pos.Ssc.SscAlgo                      (SscAlgo (..))
import           Pos.Util.TimeWarp                    (NetworkAddress, addrParser,
                                                       addrParserNoWildcard,
                                                       addressToNodeId)

data CommonArgs = CommonArgs
    { logConfig            :: !(Maybe FilePath)
    , logPrefix            :: !(Maybe FilePath)
    , reportServers        :: ![Text]
    , updateServers        :: ![Text]
    , configurationOptions :: !ConfigurationOptions
    } deriving Show

commonArgsParser :: Opt.Parser CommonArgs
commonArgsParser = do
    logConfig <- optionalLogConfig
    logPrefix <- optionalLogPrefix
    reportServers <- reportServersOption
    updateServers <- updateServersOption
    configurationOptions <- configurationOptionsParser
    pure CommonArgs{..}

-- Note: if you want to change names of these options, please also
-- update cardano-launcher accordingly (grep for these names).
configurationOptionsParser :: Opt.Parser ConfigurationOptions
configurationOptionsParser = do
    cfoFilePath    <- filePathParser
    cfoKey         <- keyParser
    cfoSystemStart <- systemStartParser
    return ConfigurationOptions{..}
  where
    filePathParser :: Opt.Parser FilePath
    filePathParser = Opt.strOption $
        Opt.long    "configuration-file" <>
        Opt.metavar "FILEPATH" <>
        Opt.help    "Path to a yaml configuration file" <>
        Opt.value   (cfoFilePath def)
    keyParser :: Opt.Parser Text
    keyParser = fmap toText $ Opt.strOption $
        Opt.long    "configuration-key" <>
        Opt.metavar "TEXT" <>
        Opt.help    "Key within the configuration file to use" <>
        Opt.value   (toString (cfoKey def))
    systemStartParser :: Opt.Parser (Maybe Timestamp)
    systemStartParser = Opt.option (Just . Timestamp . sec <$> Opt.auto) $
        Opt.long    "system-start" <>
        Opt.metavar "TIMESTAMP" <>
        Opt.help    "System start time. Format - seconds since Unix Epoch." <>
        Opt.value   (cfoSystemStart def)

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
        templateParser "log-config" "FILEPATH" "Path to logger configuration."

optionalLogPrefix :: Opt.Parser (Maybe String)
optionalLogPrefix =
    optional $ Opt.strOption $
        templateParser "logs-prefix" "FILEPATH" "Prefix to logger output path."

optionalJSONPath :: Opt.Parser (Maybe FilePath)
optionalJSONPath =
    Opt.optional $ Opt.strOption $
        templateParser "json-log" "FILEPATH" "Path to JSON log file."

portOption :: Word16 -> Opt.Parser Word16
portOption portNum =
    Opt.option Opt.auto $
        templateParser "port" "PORT" "Port to work on."
        <> Opt.value portNum
        <> Opt.showDefault

sscAlgoOption :: Opt.Parser SscAlgo
sscAlgoOption =
    Opt.option (fromParsec sscAlgoParser) $
        templateParser "ssc-algo"
                       "ALGO"
                       "Shared Seed Calculation algorithm which nodes will use."
        <> Opt.value GodTossingAlgo
        <> Opt.showDefault

reportServersOption :: Opt.Parser [Text]
reportServersOption =
    many $
    toText <$>
    Opt.strOption
        (templateParser
             "report-server"
             "URI"
             "Reporting server to send crash/error logs on.")

updateServersOption :: Opt.Parser [Text]
updateServersOption =
    many $
    toText <$>
    Opt.strOption
        (templateParser "update-server" "URI" "Server to download updates from.")

timeLordOption :: Opt.Parser Bool
timeLordOption =
    Opt.switch
        (Opt.long "time-lord" <>
         Opt.help "Peer is time lord, i.e. one responsible for system start time decision\
                  \ and propagation (used only in development mode).")

webPortOption :: Word16 -> String -> Opt.Parser Word16
webPortOption portNum help =
    Opt.option Opt.auto $
        templateParser "web-port" "PORT" help -- "Port for web server"
        <> Opt.value portNum
        <> Opt.showDefault

walletAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
walletAddressOption na =
    Opt.option (fromParsec addrParser) $
            Opt.long "wallet-address"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "IP and port for backend wallet API."

externalNetworkAddressOption :: Maybe NetworkAddress -> Opt.Parser NetworkAddress
externalNetworkAddressOption na =
    Opt.option (fromParsec addrParserNoWildcard) $
            Opt.long "address"
         <> Opt.metavar "IP:PORT"
         <> Opt.help helpMsg
         <> Opt.showDefault
         <> maybe mempty Opt.value na
  where
    helpMsg = "IP and port of external address. "
        <> "Please make sure these IP and port (on which node is running) are accessible "
        <> "otherwise proper work of CSL isn't guaranteed. "
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
    helpMsg = "IP and port on which to bind and listen. Please make sure these IP "
        <> "and port are accessible, otherwise proper work of CSL isn't guaranteed."
