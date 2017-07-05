{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module ExplorerOptions
       ( Args (..)
       , getExplorerOptions
       ) where

import           Universum                  hiding (show)

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             showDefault, simpleOptions, strOption,
                                             switch, value)
import           Prelude                    (show)
import           Serokell.Util.OptParse     (fromParsec)

import           Paths_cardano_sl_explorer  (version)
import qualified Pos.CLI                    as CLI
import           Pos.DHT.Model              (DHTKey)
import           Pos.DHT.Real.CLI           (dhtExplicitInitialOption, dhtKeyOption,
                                             dhtNetworkAddressOption, dhtPeersFileOption)
import           Pos.Statistics             (EkgParams, StatsdParams,
                                             ekgParamsOption, statsdParamsOption)
import           Pos.Util.BackupPhrase      (BackupPhrase, backupPhraseWordsNum)
import           Pos.Util.TimeWarp          (NetworkAddress, addrParser)

data Args = Args
    { dbPath             :: !FilePath
    , rebuildDB          :: !Bool
    , keyfilePath        :: !FilePath
    , backupPhrase       :: !(Maybe BackupPhrase)
    , dhtKey             :: !(Maybe DHTKey)
    , bindAddress        :: !NetworkAddress
    , externalAddress    :: !NetworkAddress
      -- ^ A node must be addressable on the network.
    , dhtNetworkAddress  :: !NetworkAddress
      -- ^ A node may have a bind address which differs from its external
      -- address.
    , dhtPeersList       :: ![NetworkAddress]
      -- ^ A list of initial Kademlia peers to use.
    , dhtExplicitInitial :: !Bool
    , dhtPeersFile       :: !(Maybe FilePath)
      -- ^ A file containing a list of Kademlia peers to use.
    , timeLord           :: !Bool
    , jlPath             :: !(Maybe FilePath)
    , kademliaDumpPath   :: !FilePath
    , webPort            :: !Word16
    , commonArgs         :: !CLI.CommonArgs
    , noSystemStart      :: !Int
    , noNTP              :: !Bool
    , enableMetrics      :: !Bool
    , ekgParams          :: !(Maybe EkgParams)
    , statsdParams       :: !(Maybe StatsdParams)
    , notifierPort       :: !Word16
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

    externalAddress <- CLI.externalNetworkAddressOption (Just ("0.0.0.0", 0))
    bindAddress <- CLI.listenNetworkAddressOption (Just ("0.0.0.0", 0))
    dhtNetworkAddress <- dhtNetworkAddressOption (Just ("0.0.0.0", 0))
    dhtExplicitInitial <- dhtExplicitInitialOption
    dhtPeersList <- many addrNodeOption
    dhtPeersFile <- optional dhtPeersFileOption
    dhtKey <- optional dhtKeyOption

    timeLord <- CLI.timeLordOption

    jlPath <- CLI.optionalJSONPath

    kademliaDumpPath <- strOption $
        long    "kademlia-dump-path" <>
        metavar "FILEPATH" <>
        value   "kademlia.dump" <>
        help    "Path to Kademlia dump file. If file doesn't exist, it will be created." <>
        showDefault

    webPort <- CLI.webPortOption 8100 "Port for web API."

    commonArgs <- CLI.commonArgsParser

    noSystemStart <- option auto (long "system-start" <> metavar "TIMESTAMP" <> value (-1))
    
    noNTP <- switch $
         long "no-ntp" <>
         help "Whether to use real NTP servers to synchronise time or rely on local time"

    notifierPort <- option auto
         (long "notifier-port" <> metavar "PORT" <>
          value 8110 <> showDefault <>
          help "Port for update notifier")

    enableMetrics <- switch $
        long "metrics" <>
        help "Enable metrics (EKG, statsd)"
 
    ekgParams <- optional ekgParamsOption
    statsdParams <- optional statsdParamsOption

    pure Args{..}

addrNodeOption :: Parser NetworkAddress
addrNodeOption =
    option (fromParsec addrParser) $
        long "kademlia-peer" <>
        metavar "HOST:PORT" <>
        help "Identifier of a node in a Kademlia network"

getExplorerOptions :: IO Args
getExplorerOptions = do
    (res, ()) <-
        simpleOptions
            ("cardano-explorer-" <> showVersion version)
            "CardanoSL explorer node"
            "CardanoSL explorer node."
            argsParser
            empty
    pure res
