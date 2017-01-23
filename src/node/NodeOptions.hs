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
    }
  deriving Show

argsParser :: Parser Args
argsParser =
    Args <$>
    strOption
        (long "db-path" <> metavar "FILEPATH" <> value "node-db" <>
        help "Path to the node database") <*>
    switch
        (long "rebuild-db" <>
         help
             "If we DB already exist, discard it's contents and create new one from\
             \ scratch") <*>
#ifdef DEV_MODE
    optional
        (option
             auto
             (long "spending-genesis" <> metavar "INT" <>
              help "Use genesis spending #i")) <*>
    optional
        (option
             auto
             (long "vss-genesis" <> metavar "INT" <> help "Use genesis vss #i")) <*>
#endif
    strOption
        (long "keyfile" <>
         metavar "FILEPATH" <>
         value "secret.key" <>
         help "Path to file with secret keys") <*>
    optional
        (option auto $
            long "backup-phrase" <>
            metavar "PHRASE" <>
            help (show backupPhraseWordsNum ++ "-word phrase to recover the wallet")) <*>
    CLI.ipPortOption ("0.0.0.0", 3000) <*>
    switch
        (long "supporter" <> help "Launch DHT supporter instead of full node") <*>
    optional
        (option (fromParsec CLI.dhtKeyParser) $
         long "dht-key" <> metavar "HOST_ID" <> help "DHT key in base64-url") <*>
    CLI.timeLordOption <*>
    switch (long "stats" <> help "Enable stats logging") <*>
    CLI.optionalJSONPath <*>
    many
        (option (fromParsec CLI.attackTypeParser) $
         long "attack" <> metavar "NoBlocks|NoCommitments"
         <> help "Attack type to emulate") <*>
    many
        (option (fromParsec CLI.attackTargetParser) $
         long "attack-target" <> metavar "HOST:PORT|PUBKEYHASH") <*>
    strOption
        (long "kademlia-dump-path" <> metavar "FILEPATH" <> showDefault <>
        help "Path to kademlia dump file" <> value "kademlia.dump")
#ifdef WITH_WEB
    <*>
    switch
        (long "web" <>
         help "Run web server") <*>
    CLI.webPortOption 8080 "Port for web server"
#ifdef WITH_WALLET
    <*>
    switch
        (long "wallet" <>
         help "Run wallet web api") <*>
    CLI.webPortOption 8090 "Port for Daedalus Wallet API" <*>
    strOption
        (long "wallet-db-path" <>
         help "Path to the wallet acid-state" <>
         value "wallet-db") <*>
    switch
        (long "wallet-rebuild-db" <>
         help "If the wallet DB already exist, discard it's contents and create \
              \new one from scratch") <*>
    switch
        (long "wallet-debug" <>
         help "Run wallet with debug params (e. g. include all the genesis keys in the set of secret keys)")
#endif
#endif
    <*> CLI.commonArgsParser peerHelpMsg
    <*> option auto (long "system-start" <> metavar "TIMESTAMP" <> value (-1))
  where
    peerHelpMsg =
        "Peer to connect to for initial peer discovery. Format\
        \ example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

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
