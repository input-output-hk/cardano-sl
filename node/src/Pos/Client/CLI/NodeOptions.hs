{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of pos-node.

module Pos.Client.CLI.NodeOptions
       ( CommonNodeArgs (..)
       , SimpleNodeArgs (..)
       , NodeArgs (..)
       , commonNodeArgsParser
       , getSimpleNodeOptions
       , usageExample
       ) where

import           Universum                    hiding (show)

import           Data.Version                 (showVersion)
import           NeatInterpolation            (text)
import           Options.Applicative          (Parser, auto, execParser, footerDoc,
                                               fullDesc, header, help, helper, info,
                                               infoOption, long, metavar, option,
                                               progDesc, showDefault, strOption, switch,
                                               value)
import           Prelude                      (show)
import           Serokell.Util.OptParse       (fromParsec)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl             (version)

import           Pos.Client.CLI.Options       (CommonArgs (..), commonArgsParser,
                                               externalNetworkAddressOption,
                                               listenNetworkAddressOption,
                                               optionalJSONPath, sscAlgoOption)
import           Pos.Constants                (isDevelopment)
import           Pos.Network.CLI              (NetworkConfigOpts, networkConfigOption)
import           Pos.Network.Types            (NodeId, NodeType (..))
import           Pos.Ssc.SscAlgo              (SscAlgo (..))
import           Pos.Statistics               (EkgParams, StatsdParams, ekgParamsOption,
                                               statsdParamsOption)
import           Pos.Util.BackupPhrase        (BackupPhrase, backupPhraseWordsNum)
import           Pos.Util.TimeWarp            (NetworkAddress, addrParser,
                                               addressToNodeId)

data CommonNodeArgs = CommonNodeArgs
    { dbPath                 :: !FilePath
    , rebuildDB              :: !Bool
    -- these two arguments are only used in development mode
    , devSpendingGenesisI    :: !(Maybe Int)
    , devVssGenesisI         :: !(Maybe Int)
    , keyfilePath            :: !FilePath
    , backupPhrase           :: !(Maybe BackupPhrase)
    , externalAddress        :: !(Maybe NetworkAddress)
      -- ^ A node must be addressable on the network.
    , bindAddress            :: !(Maybe NetworkAddress)
      -- ^ A node may have a bind address which differs from its external
      -- address.
    , peers                  :: ![(NodeId, NodeType)]
      -- ^ Known peers (addresses with classification).
    , networkConfigOpts      :: !NetworkConfigOpts
      -- ^ Network configuration
    , jlPath                 :: !(Maybe FilePath)
    , kademliaDumpPath       :: !FilePath
    , commonArgs             :: !CommonArgs
    , updateLatestPath       :: !FilePath
    , updateWithPackage      :: !Bool
    , noNTP                  :: !Bool
    , enableMetrics          :: !Bool
    , ekgParams              :: !(Maybe EkgParams)
    , statsdParams           :: !(Maybe StatsdParams)
    , cnaDumpGenesisDataPath :: !(Maybe FilePath)
    } deriving Show

commonNodeArgsParser :: Parser CommonNodeArgs
commonNodeArgsParser = do
    dbPath <- strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        value   "node-db" <>
        help    "Path to directory with all DBs used by the node. \
                \If specified path doesn’t exist, a directory will be created."
    rebuildDB <- switch $
        long "rebuild-db" <>
        help "If node's database already exists, discard its contents \
             \and create a new one from scratch."
    devSpendingGenesisI <- if isDevelopment
        then (optional $ option auto $
                  long    "spending-genesis" <>
                  metavar "INT" <>
                  help    "Used genesis secret key index.")
        else pure Nothing
    devVssGenesisI <- if isDevelopment
        then (optional $ option auto $
                  long    "vss-genesis" <>
                  metavar "INT" <>
                  help    "Index of using VSS key pair in genesis.")
        else pure Nothing
    keyfilePath <- strOption $
        long    "keyfile" <>
        metavar "FILEPATH" <>
        value   "secret.key" <>
        help    "Path to file with secret key (we use it for Daedalus)."
    backupPhrase <- optional $ option auto $
        long    "backup-phrase" <>
        metavar "PHRASE" <>
        help    (show backupPhraseWordsNum ++
                 "-word phrase to recover the wallet. Words should be separated by spaces.")
    externalAddress <-
        optional $ externalNetworkAddressOption Nothing
    bindAddress <-
        optional $ listenNetworkAddressOption Nothing
    peers <- (++) <$> corePeersList <*> relayPeersList
    networkConfigOpts <- networkConfigOption
    jlPath <-
        optionalJSONPath
    kademliaDumpPath <- strOption $
        long    "kademlia-dump-path" <>
        metavar "FILEPATH" <>
        value   "kademlia.dump" <>
        help    "Path to Kademlia dump file. If file doesn't exist, it will be created." <>
        showDefault
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
    noNTP <- switch $
        long "no-ntp" <>
        help "Whether to use real NTP servers to synchronise time or rely on local time"

    enableMetrics <- switch $
        long "metrics" <>
        help "Enable metrics (EKG, statsd)"

    ekgParams <- optional ekgParamsOption
    statsdParams <- optional statsdParamsOption

    cnaDumpGenesisDataPath <- optional $ strOption $
        long "dump-genesis-data-to" <>
        help "Dump genesis data in canonical JSON format to this file."

    pure CommonNodeArgs{..}
  where
    corePeersList = many (peerOption "peer-core" (flip (,) NodeCore . addressToNodeId))
    relayPeersList = many (peerOption "peer-relay" (flip (,) NodeRelay . addressToNodeId))

data SimpleNodeArgs = SimpleNodeArgs CommonNodeArgs NodeArgs

data NodeArgs = NodeArgs
    { sscAlgo            :: !SscAlgo
    , behaviorConfigPath :: !(Maybe FilePath)
    } deriving Show

simpleNodeArgsParser :: Parser SimpleNodeArgs
simpleNodeArgsParser = do
    commonNodeArgs <- commonNodeArgsParser
    sscAlgo <- sscAlgoOption
    behaviorConfigPath <- behaviorConfigOption
    pure $ SimpleNodeArgs commonNodeArgs NodeArgs{..}

behaviorConfigOption :: Parser (Maybe FilePath)
behaviorConfigOption =
    optional $ strOption $
        long "behavior" <>
        metavar "FILE" <>
        help "Path to the behavior config"

peerOption :: String -> (NetworkAddress -> (NodeId, NodeType)) -> Parser (NodeId, NodeType)
peerOption longName mk =
    option (fromParsec (mk <$> addrParser)) $
        long longName <>
        metavar "HOST:PORT" <>
        help "Address of a peer"

getSimpleNodeOptions :: IO SimpleNodeArgs
getSimpleNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> simpleNodeArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node."
                 <> header "Cardano SL node."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-node-" <> showVersion version)
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
