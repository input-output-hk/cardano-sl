{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of pos-node.

module NodeOptions
       ( Args (..)
       , getNodeOptions
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
import qualified Text.Parsec.Char             as P
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl             (version)
import qualified Pos.CLI                      as CLI
import           Pos.Constants                (isDevelopment)
import           Pos.Network.CLI              (NetworkConfigOpts, networkConfigOption)
import           Pos.Network.Types            (NodeId, NodeType (..))
import           Pos.Security                 (AttackTarget, AttackType)
import           Pos.Statistics               (EkgParams, StatsdParams, ekgParamsOption,
                                               statsdParamsOption)
import           Pos.Util.BackupPhrase        (BackupPhrase, backupPhraseWordsNum)
import           Pos.Util.TimeWarp            (NetworkAddress, addrParser,
                                               addressToNodeId)
import           Pos.Web                      (TlsParams)

data Args = Args
    { dbPath                    :: !FilePath
    , rebuildDB                 :: !Bool
    -- these two arguments are only used in development mode
    , devSpendingGenesisI       :: !(Maybe Int)
    , devVssGenesisI            :: !(Maybe Int)
    , keyfilePath               :: !FilePath
    , backupPhrase              :: !(Maybe BackupPhrase)
    , externalAddress           :: !NetworkAddress
      -- ^ A node must be addressable on the network.
    , bindAddress               :: !NetworkAddress
      -- ^ A node may have a bind address which differs from its external
      -- address.
    , supporterNode             :: !Bool
    , nodeType                  :: !NodeType
    , peers                     :: ![(NodeId, NodeType)]
      -- ^ Known peers (addresses with classification).
    , networkConfigOpts         :: !NetworkConfigOpts
      -- ^ Network configuration
    , jlPath                    :: !(Maybe FilePath)
    , maliciousEmulationAttacks :: ![AttackType]
    , maliciousEmulationTargets :: ![AttackTarget]
    , kademliaDumpPath          :: !FilePath
#ifdef WITH_WEB
    , enableWeb                 :: !Bool
    , webPort                   :: !Word16
    , walletTLSParams           :: !TlsParams
#ifdef WITH_WALLET
    , enableWallet              :: !Bool
    , walletPort                :: !Word16
    , walletDbPath              :: !FilePath
    , walletRebuildDb           :: !Bool
    , walletDebug               :: !Bool
#endif
#endif
    , commonArgs                :: !CLI.CommonArgs
    , updateLatestPath          :: !FilePath
    , updateWithPackage         :: !Bool
    , noNTP                     :: !Bool
    , enableMetrics             :: !Bool
    , ekgParams                 :: !(Maybe EkgParams)
    , statsdParams              :: !(Maybe StatsdParams)
    } deriving Show

argsParser :: Parser Args
argsParser = do
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
        CLI.externalNetworkAddressOption (Just ("0.0.0.0", 0))
    bindAddress <-
        CLI.listenNetworkAddressOption (Just ("0.0.0.0", 0))
    supporterNode <- switch $
        long "supporter" <>
        help "Launch DHT supporter instead of full node"
    nodeType <- nodeTypeOption
    peers <- (++) <$> corePeersList <*> relayPeersList
    networkConfigOpts <- networkConfigOption
    jlPath <-
        CLI.optionalJSONPath
    maliciousEmulationAttacks <-
        many $ option (fromParsec CLI.attackTypeParser) $
        long    "attack" <>
        metavar "NoBlocks | NoCommitments" <>
        help    "Attack type to emulate. This option can be defined more than once."
    maliciousEmulationTargets <-
        many $ option (fromParsec CLI.attackTargetParser) $
        long    "attack-target" <>
        metavar "HOST:PORT | PUBKEYHASH" <>
        help    "Node for attack. This option can be defined more than once."
    kademliaDumpPath <- strOption $
        long    "kademlia-dump-path" <>
        metavar "FILEPATH" <>
        value   "kademlia.dump" <>
        help    "Path to Kademlia dump file. If file doesn't exist, it will be created." <>
        showDefault
#ifdef WITH_WEB
    enableWeb <- switch $
        long "web" <>
        help "Activate web API (it’s not linked with a wallet web API)."
    webPort <-
        CLI.webPortOption 8080 "Port for web API."
    walletTLSParams <- CLI.tlsParamsOption
#ifdef WITH_WALLET
    enableWallet <- switch $
        long "wallet" <>
        help "Activate Wallet web API."
    walletPort <-
        CLI.walletPortOption 8090 "Port for Daedalus Wallet API."
    walletDbPath <- strOption $
        long  "wallet-db-path" <>
        help  "Path to the wallet's database." <>
        value "wallet-db"
    walletRebuildDb <- switch $
        long "wallet-rebuild-db" <>
        help "If wallet's database already exists, discard its contents \
             \and create a new one from scratch."
    walletDebug <- switch $
        long "wallet-debug" <>
        help "Run wallet with debug params (e.g. include \
             \all the genesis keys in the set of secret keys)."
#endif
#endif
    commonArgs <- CLI.commonArgsParser
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

    pure Args{..}
  where
    corePeersList = many (peerOption "peer-core" (flip (,) NodeCore . addressToNodeId))
    relayPeersList = many (peerOption "peer-relay" (flip (,) NodeRelay . addressToNodeId))

nodeTypeOption :: Parser NodeType
nodeTypeOption =
    option (fromParsec nodeTypeParser) $
        long "node-type" <>
        value NodeCore <>
        metavar "core|relay|edge" <>
        help "The type of this node (core, relay, edge), default core"
  where
    nodeTypeParser =
            (NodeCore  <$ P.string "core")
        <|> (NodeRelay <$ P.string "relay")
        <|> (NodeEdge  <$ P.string "edge")

peerOption :: String -> (NetworkAddress -> (NodeId, NodeType)) -> Parser (NodeId, NodeType)
peerOption longName mk =
    option (fromParsec (mk <$> addrParser)) $
        long longName <>
        metavar "HOST:PORT" <>
        help "Address of a peer"

getNodeOptions :: IO Args
getNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> argsParser) $
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
