{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of Cardano node.

module Pos.Client.CLI.NodeOptions
       ( CommonNodeArgs (..)
       , SimpleNodeArgs (..)
       , NodeArgs (..)
       , commonNodeArgsParser
       , getSimpleNodeOptions
       , usageExample
       ) where

import           Universum

import           Data.Version (showVersion)
import           NeatInterpolation (text)
import           Options.Applicative (Parser, auto, execParser, footerDoc,
                     fullDesc, header, help, helper, info, infoOption, long,
                     metavar, option, progDesc, strOption, switch, value)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl (version)

import           Pos.Client.CLI.Options (CommonArgs (..), commonArgsParser,
                     optionalJSONPath)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.HealthCheck.Route53 (route53HealthCheckOption)
import           Pos.Infra.Network.CLI (NetworkConfigOpts, networkConfigOption)
import           Pos.Infra.Statistics (EkgParams, StatsdParams, ekgParamsOption,
                     statsdParamsOption)
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                     compileInfo)

data CommonNodeArgs = CommonNodeArgs
    { dbPath                 :: !(Maybe FilePath)
    , rebuildDB              :: !Bool
    , cnaAssetLockPath       :: !(Maybe FilePath)
    -- these two arguments are only used in development mode
    , devGenesisSecretI      :: !(Maybe Int)
    , publicKeyfilePath      :: !FilePath
    , keyfilePath            :: !FilePath
    , networkConfigOpts      :: !NetworkConfigOpts
      -- ^ Network configuration
    , jlPath                 :: !(Maybe FilePath)
    , commonArgs             :: !CommonArgs
    , updateLatestPath       :: !FilePath
    , updateWithPackage      :: !Bool
    , route53Params          :: !(Maybe NetworkAddress)
    , enableMetrics          :: !Bool
    , ekgParams              :: !(Maybe EkgParams)
    , statsdParams           :: !(Maybe StatsdParams)
    , cnaDumpGenesisDataPath :: !(Maybe FilePath)
    , cnaDumpConfiguration   :: !Bool
    } deriving Show

commonNodeArgsParser :: Parser CommonNodeArgs
commonNodeArgsParser = do
    dbPath <- optional $ strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        help    "Path to directory with all DBs used by the node. \
                \If specified path doesn't exist, a directory will be created."
    rebuildDB <- switch $
        long "rebuild-db" <>
        help "If node's database already exists, discard its contents \
             \and create a new one from scratch."

    cnaAssetLockPath <- optional $ strOption $
        long    "asset-lock-file" <>
        metavar "FILEPATH" <>
        help    "Path to list of assetLocked source addresses. Funds at these \
                \addresses are not able to be spent. This will only be effective \
                \while Cardano is centrally mined/minted. Addresses should be listed \
                \one per line. Lines beginning with '#' are comments."

    devGenesisSecretI <-
        optional $ option auto $
                  long    "genesis-secret" <>
                  metavar "INT" <>
                  help    "Used genesis secret key index."
    publicKeyfilePath <- strOption $
        long    "pubkeyfile" <>
        metavar "FILEPATH" <>
        value   "public.key" <>
        help    "Path to file with public key (we use it for external wallets)."
    keyfilePath <- strOption $
        long    "keyfile" <>
        metavar "FILEPATH" <>
        value   "secret.key" <>
        help    "Path to file with secret key (we use it for Daedalus)."
    networkConfigOpts <- networkConfigOption
    jlPath <-
        optionalJSONPath
    commonArgs <- commonArgsParser
    updateLatestPath <- strOption $
        long    "update-latest-path" <>
        metavar "FILEPATH" <>
        value   "update-installer.exe" <>
        help    "Path to update installer file, \
                \which should be downloaded by Update System."
    updateWithPackage <- switch $
        long "update-with-package" <>
        help "Enable updating via installer."

    route53Params <- optional route53HealthCheckOption

    enableMetrics <- switch $
        long "metrics" <>
        help "Enable metrics (EKG, statsd)"

    ekgParams <- optional ekgParamsOption
    statsdParams <- optional statsdParamsOption

    cnaDumpGenesisDataPath <- optional $ strOption $
        long "dump-genesis-data-to" <>
        help "Dump genesis data in canonical JSON format to this file."

    cnaDumpConfiguration <- switch $
        long "dump-configuration" <>
        help "Dump configuration and exit."

    pure CommonNodeArgs{..}

data SimpleNodeArgs = SimpleNodeArgs CommonNodeArgs NodeArgs

data NodeArgs = NodeArgs
    { behaviorConfigPath :: !(Maybe FilePath)
    } deriving Show

simpleNodeArgsParser :: Parser SimpleNodeArgs
simpleNodeArgsParser = do
    commonNodeArgs <- commonNodeArgsParser
    behaviorConfigPath <- behaviorConfigOption
    pure $ SimpleNodeArgs commonNodeArgs NodeArgs{..}

behaviorConfigOption :: Parser (Maybe FilePath)
behaviorConfigOption =
    optional $ strOption $
        long "behavior" <>
        metavar "FILE" <>
        help "Path to the behavior config"

getSimpleNodeOptions :: HasCompileInfo => IO SimpleNodeArgs
getSimpleNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> simpleNodeArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node."
                 <> header "Cardano SL node."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-node-" <> showVersion version <>
         ", git revision " <> toString (ctiGitRevision compileInfo))
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [text|
Command example:

  stack exec -- cardano-node                                             \
    --db-path node-db0                                                   \
    --rebuild-db                                                         \
    --keyfile secrets/secret-1.key                                       \
    --kademlia-id a_P8zb6fNP7I2H54FtGuhqxaMDAwMDAwMDAwMDAwMDA=           \
    --address 127.0.0.1:3000                                             \
    --listen 127.0.0.1:3000                                              \
    --kademlia-address 127.0.0.1:3000                                    \
    --json-log=/tmp/logs/2017-05-22_181224/node0.json                    \
    --logs-prefix /tmp/logs/2017-05-22_181224                            \
    --log-config /tmp/logs/2017-05-22_181224/conf/node0.log.yaml         \
    --kademlia-dump-path /tmp/logs/2017-05-22_181224/dump/kademlia0.dump \
    --system-start 1495462345|]
