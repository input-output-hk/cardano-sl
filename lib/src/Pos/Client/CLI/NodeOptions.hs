{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE CPP             #-}
{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

-- | Command line options of Cardano node.

module Pos.Client.CLI.NodeOptions
       ( CommonNodeArgs (..)
       , SimpleNodeArgs (..)
       , NodeArgs (..)
       , NodeApiArgs (..)
       , NodeWithApiArgs (..)
       , commonNodeArgsParser
       , nodeArgsParser
       , nodeApiArgsParser
       , getSimpleNodeOptions
       , getNodeApiOptions
       , usageExample

       , dbPath_L
       , rebuildDB_L
       , cnaAssetLockPath_L
       , devGenesisSecretI_L
       , publicKeyfilePath_L
       , keyfilePath_L
       , networkConfigOpts_L
       , jlPath_L
       , commonArgs_L
       , updateLatestPath_L
       , updateWithPackage_L
       , route53Params_L
       , enableMetrics_L
       , ekgParams_L
       , statsdParams_L
       , cnaDumpGenesisDataPath_L
       , cnaDumpConfiguration_L
       , cnaFInjectsSpec_L
       ) where

import           Universum

import           Control.Lens (makeLensesWith)
import           Data.Version (showVersion)
import           NeatInterpolation (text)
import           Options.Applicative (Parser, auto, execParser, footerDoc,
                     fullDesc, header, help, helper, info, infoOption, long,
                     metavar, option, progDesc, strOption, switch, value)
import           Pos.Util (postfixLFields)

import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl (version)

import           Pos.Client.CLI.Options (CommonArgs (..), commonArgsParser,
                     optionalJSONPath, templateParser)
import           Pos.Core.NetworkAddress (NetworkAddress, addrParser)
import           Pos.Infra.HealthCheck.Route53 (route53HealthCheckOption)
import           Pos.Infra.InjectFail (FInjectsSpec, parseFInjectsSpec)
import           Pos.Infra.Network.CLI (NetworkConfigOpts, networkConfigOption)
import           Pos.Infra.Statistics (EkgParams, StatsdParams, ekgParamsOption,
                     statsdParamsOption)
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo,
                     compileInfo)
import           Pos.Util.OptParse (fromParsec)
import           Pos.Web (TlsParams (..))

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
    , cnaFInjectsSpec        :: !FInjectsSpec
    } deriving Show

makeLensesWith postfixLFields ''CommonNodeArgs

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

    cnaFInjectsSpec <- parseFInjectsSpec
        -- The fault injection CLI switches are generated in infra/src/Pos/Infra/InjectFail.hs
        -- from the FInject ADT, by:  1) lowercasing and 2) dropping the 4-letter prefix.
        -- I.e.
        --     FInjIgnoreShutdown → --ignoreshutdown
        -- Additionally, a single extra flag is required to allow fault injection processing at all:
        --   --allow-fault-injection

    pure CommonNodeArgs{..}

data SimpleNodeArgs = SimpleNodeArgs CommonNodeArgs NodeArgs

data NodeArgs = NodeArgs
    { behaviorConfigPath   :: !(Maybe FilePath)
    } deriving Show

simpleNodeArgsParser :: Parser SimpleNodeArgs
simpleNodeArgsParser =
    SimpleNodeArgs <$> commonNodeArgsParser <*> nodeArgsParser

nodeArgsParser :: Parser NodeArgs
nodeArgsParser = NodeArgs <$> behaviorParser
  where
    behaviorParser =
        optional $ strOption $
            long "behavior" <>
            metavar "FILE" <>
            help "Path to the behavior config"

data NodeWithApiArgs = NodeWithApiArgs CommonNodeArgs NodeArgs (Maybe NodeApiArgs)

nodeWithApiArgsParser :: Parser NodeWithApiArgs
nodeWithApiArgsParser =
    NodeWithApiArgs
        <$> commonNodeArgsParser
        <*> nodeArgsParser
        <*> nodeApiArgsParser

nodeApiArgsParser :: Parser (Maybe NodeApiArgs)
nodeApiArgsParser = optional $ NodeApiArgs
    <$> addressParser "node-api-address"
    <*> tlsParamsParser
    <*> debugModeParser
    <*> addressParser "node-doc-address"
  where
    addressParser :: String -> Parser NetworkAddress
    addressParser flagName =
        option (fromParsec addrParser) $
            long flagName
         <> metavar "IP:PORT"
         <> help "IP and port for backend node API."
    debugModeParser :: Parser Bool
    debugModeParser =
        switch (long "wallet-debug" <>
                help "Run wallet with debug params (e.g. include \
                     \all the genesis keys in the set of secret keys)."
               )

data NodeApiArgs = NodeApiArgs
    { nodeBackendAddress    :: !NetworkAddress
    , nodeBackendTLSParams  :: !(Maybe TlsParams)
    , nodeBackendDebugMode  :: !Bool
    , nodeBackendDocAddress :: !NetworkAddress
    } deriving Show

tlsParamsParser :: Parser (Maybe TlsParams)
tlsParamsParser = constructTlsParams <$> certPathParser
                                     <*> keyPathParser
                                     <*> caPathParser
                                     <*> (not <$> noClientAuthParser)
                                     <*> disabledParser
  where
    constructTlsParams tpCertPath tpKeyPath tpCaPath tpClientAuth disabled =
        guard (not disabled) $> TlsParams{..}

    certPathParser :: Parser FilePath
    certPathParser = strOption (templateParser
                                "tlscert"
                                "FILEPATH"
                                "Path to file with TLS certificate"
                               )

    keyPathParser :: Parser FilePath
    keyPathParser = strOption (templateParser
                               "tlskey"
                               "FILEPATH"
                               "Path to file with TLS key"
                              )

    caPathParser :: Parser FilePath
    caPathParser = strOption (templateParser
                              "tlsca"
                              "FILEPATH"
                              "Path to file with TLS certificate authority"
                             )

    noClientAuthParser :: Parser Bool
    noClientAuthParser = switch $
                         long "no-client-auth" <>
                         help "Disable TLS client verification. If turned on, \
                              \no client certificate is required to talk to \
                              \the API."

    disabledParser :: Parser Bool
    disabledParser = switch $
                     long "no-tls" <>
                     help "Disable tls. If set, 'tlscert', 'tlskey' \
                          \and 'tlsca' options are ignored"

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

getNodeApiOptions :: HasCompileInfo => IO NodeWithApiArgs
getNodeApiOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> nodeWithApiArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node with API."
                 <> header "Cardano SL node with API."
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
