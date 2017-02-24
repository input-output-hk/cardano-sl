{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module NodeOptions
       ( Args (..)
       , getNodeOptions
       ) where

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             showDefault, simpleOptions, strOption,
                                             switch, value)
import           Prelude                    (show)
import           Serokell.Util.OptParse     (fromParsec)
import           Universum                  hiding (show)

import           Paths_cardano_sl           (version)
import qualified Pos.CLI                    as CLI
import           Pos.DHT.Model              (DHTKey)
import           Pos.Security.CLI           (AttackTarget, AttackType)
import           Pos.Util.BackupPhrase      (BackupPhrase, backupPhraseWordsNum)
import           Pos.Util.TimeWarp          (NetworkAddress)

data Args = Args
    { dbPath                    :: !FilePath
    , rebuildDB                 :: !Bool
#ifdef DEV_MODE
    , spendingGenesisI          :: !(Maybe Int)
    , vssGenesisI               :: !(Maybe Int)
#endif
    , keyfilePath               :: !FilePath
    , backupPhrase              :: !(Maybe BackupPhrase)
    , ipPort                    :: !NetworkAddress
    , supporterNode             :: !Bool
    , dhtKey                    :: !(Maybe DHTKey)
    , timeLord                  :: !Bool
    , enableStats               :: !Bool
    , jlPath                    :: !(Maybe FilePath)
    , maliciousEmulationAttacks :: ![AttackType]
    , maliciousEmulationTargets :: ![AttackTarget]
    , kademliaDumpPath          :: !FilePath
#ifdef WITH_WEB
    , enableWeb                 :: !Bool
    , webPort                   :: !Word16
#ifdef WITH_WALLET
    , enableWallet              :: !Bool
    , walletPort                :: !Word16
    , walletDbPath              :: !FilePath
    , walletRebuildDb           :: !Bool
    , walletDebug               :: !Bool
#endif
#endif
    , commonArgs                :: !CLI.CommonArgs
    , noSystemStart             :: !Int
    , updateLatestPath          :: !FilePath
    , updateWithPackage         :: !Bool
    , monitorPort               :: !(Maybe Int)
    }
  deriving Show

argsParser :: Parser Args
argsParser = do
    dbPath <- strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        value   "node-db" <>
        help    "Path to the node database"
    rebuildDB <- switch $
        long "rebuild-db" <>
        help "If we DB already exist, discard its contents \
             \and create a new one from scratch"
#ifdef DEV_MODE
    spendingGenesisI <- optional $ option auto $
        long    "spending-genesis" <>
        metavar "INT" <>
        help    "Use genesis spending #i"
    vssGenesisI <- optional $ option auto $
        long    "vss-genesis" <>
        metavar "INT" <>
        help    "Use genesis vss #i"
#endif
    keyfilePath <- strOption $
        long    "keyfile" <>
        metavar "FILEPATH" <>
        value   "secret.key" <>
        help    "Path to file with secret keys"
    backupPhrase <- optional $ option auto $
        long    "backup-phrase" <>
        metavar "PHRASE" <>
        help    (show backupPhraseWordsNum ++
                 "-word phrase to recover the wallet")
    ipPort <-
        CLI.ipPortOption ("0.0.0.0", 3000)
    supporterNode <- switch $
        long "supporter" <>
        help "Launch DHT supporter instead of full node"
    dhtKey <- optional $ option (fromParsec CLI.dhtKeyParser) $
        long    "dht-key" <>
        metavar "HOST_ID" <>
        help    "DHT key in base64-url"
    timeLord <-
        CLI.timeLordOption
    enableStats <- switch $
        long "stats" <>
        help "Enable stats logging"
    jlPath <-
        CLI.optionalJSONPath
    maliciousEmulationAttacks <-
        many $ option (fromParsec CLI.attackTypeParser) $
        long    "attack" <>
        metavar "NoBlocks|NoCommitments" <>
        help    "Attack type to emulate"
    maliciousEmulationTargets <-
        many $ option (fromParsec CLI.attackTargetParser) $
        long    "attack-target" <>
        metavar "HOST:PORT|PUBKEYHASH"
    kademliaDumpPath <- strOption $
        long    "kademlia-dump-path" <>
        metavar "FILEPATH" <>
        value   "kademlia.dump" <>
        help    "Path to kademlia dump file" <>
        showDefault
#ifdef WITH_WEB
    enableWeb <- switch $
        long "web" <>
        help "Run web server"
    webPort <-
        CLI.webPortOption 8080 "Port for web server"
#ifdef WITH_WALLET
    enableWallet <- switch $
        long "wallet" <>
        help "Run wallet web api"
    walletPort <-
        CLI.walletPortOption 8090 "Port for Daedalus Wallet API"
    walletDbPath <- strOption $
        long  "wallet-db-path" <>
        help  "Path to the wallet acid-state" <>
        value "wallet-db"
    walletRebuildDb <- switch $
        long "wallet-rebuild-db" <>
        help "If the wallet DB already exist, discard its contents \
             \and create a new one from scratch"
    walletDebug <- switch $
        long "wallet-debug" <>
        help "Run wallet with debug params (e.g. include \
             \all the genesis keys in the set of secret keys)"
#endif
#endif
    commonArgs <-
        CLI.commonArgsParser peerHelpMsg
    noSystemStart <- option auto $
        long    "system-start" <>
        metavar "TIMESTAMP" <>
        value   (-1)
    updateLatestPath <- strOption $
        long    "update-latest-path" <>
        metavar "FILEPATH" <>
        value   "update-installer.exe" <>
        help    "Path to update installer file,\
                \which should be downloaded by update system"
    updateWithPackage <- switch $
        long "update-with-package" <>
        help "Use updating via installer"
    monitorPort <- optional $ option auto $
        long    "monitor-port" <>
        metavar "INT" <>
        help    "Run web monitor on this port"

    pure Args{..}
  where
    peerHelpMsg =
        "Peer to connect to for initial peer discovery. Format\
        \ example: \"localhost:1234/dYGuDj0BrJxCsTC9ntJE7ePT7wUoVdQMH3sKLzQD8bo=\""

getNodeOptions :: IO Args
getNodeOptions = do
    (res, ()) <-
        simpleOptions
            ("cardano-node-" <> showVersion version)
            "CardanoSL node"
            "CardanoSL main server node."
            argsParser
            empty
    return res
