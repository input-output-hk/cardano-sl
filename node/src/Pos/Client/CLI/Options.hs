{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}

-- | Module for command-line options and flags

module Pos.Client.CLI.Options
       ( CommonArgs (..)
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
       , templateParser
       , sscAlgoOption

       , sysStartOption
       , nodeIdOption

       , configInfoParser
       ) where

import           Universum

import qualified Options.Applicative                  as Opt
import           Options.Applicative.Builder.Internal (HasMetavar, HasName)
import           Serokell.Util                        (sec)
import           Serokell.Util.OptParse               (fromParsec)

import           Pos.Binary.Core                      ()
import           Pos.Client.CLI.Util                  (sscAlgoParser)
import           Pos.Communication                    (NodeId)
import           Pos.Constants                        (isDevelopment, staticSysStart)
import           Pos.Core                             (Timestamp (..))
import           Pos.Launcher.ConfigInfo              (ConfigInfo (..))
import           Pos.Ssc.SscAlgo                      (SscAlgo (..))
import           Pos.Util.TimeWarp                    (NetworkAddress, addrParser,
                                                       addrParserNoWildcard,
                                                       addressToNodeId)

data CommonArgs = CommonArgs
    { logConfig     :: !(Maybe FilePath)
    , logPrefix     :: !(Maybe FilePath)
    , reportServers :: ![Text]
    , updateServers :: ![Text]
    -- distributions, only used in dev mode
    , sysStart      :: !Timestamp
      -- ^ The system start time.
    } deriving Show

commonArgsParser :: Opt.Parser CommonArgs
commonArgsParser = do
    logConfig <- optionalLogConfig
    logPrefix <- optionalLogPrefix
    --
    reportServers <- reportServersOption
    updateServers <- updateServersOption
    --
    sysStart <- sysStartParser
    pure CommonArgs{..}

sysStartParser :: Opt.Parser Timestamp
sysStartParser = Opt.option (Timestamp . sec <$> Opt.auto) $
    Opt.long    "system-start" <>
    Opt.metavar "TIMESTAMP" <>
    Opt.help    helpMsg <>
    defaultValue
  where
    -- In development mode, this parameter is mandatory.
    -- In production mode, it is optional, and its default value is populated
    -- from `staticSysStart`, which gets it from the config file.
    defaultValue =
        if isDevelopment then mempty else Opt.value staticSysStart
    helpMsg = "System start time. Mandatory in development mode. Format - seconds since Unix-epoch."

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

sysStartOption :: Opt.Parser Timestamp
sysStartOption = Opt.option (Timestamp . sec <$> Opt.auto) $
    Opt.long    "system-start" <>
    Opt.metavar "TIMESTAMP" <>
    Opt.value   staticSysStart <>
    Opt.help    helpMsg
  where
    helpMsg = "System start time. Format - seconds since Unix Epoch."

configInfoParser :: Opt.Parser ConfigInfo
configInfoParser = do
    customConfigPath <- optional $ Opt.strOption $
        Opt.long    "custom-config-file" <>
        Opt.metavar "FILEPATH" <>
        Opt.help    "Path to constants.yaml"
    customConfigName <- optional $ fmap toText $ Opt.strOption $
        Opt.long    "custom-config-name" <>
        Opt.metavar "KEY" <>
        Opt.help    "Section of constants.yaml to use"
    customGenSpecPath <- optional $ Opt.strOption $
        Opt.long    "custom-genesis-spec-yaml" <>
        Opt.metavar "FILEPATH" <>
        Opt.help    "Path to genesis-spec.yaml"
    pure ConfigInfo{..}
