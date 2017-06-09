{-# LANGUAGE ApplicativeDo       #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module ExplorerOptions
       ( Args (..)
       , getExplorerOptions
       ) where

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar,
                                             option, showDefault, simpleOptions,
                                             strOption, switch, value)
import           Prelude                    (show)
import           Universum                  hiding (show)

import           Paths_cardano_sl_explorer  (version)
import qualified Pos.CLI                    as CLI
import           Pos.DHT.Model              (DHTKey)
import           Pos.DHT.Real.CLI           (dhtKeyOption)
import           Pos.Util.BackupPhrase      (BackupPhrase, backupPhraseWordsNum)
import           Pos.Util.TimeWarp          (NetworkAddress)


data Args = Args
    { dbPath           :: !FilePath
    , rebuildDB        :: !Bool
    , keyfilePath      :: !FilePath
    , backupPhrase     :: !(Maybe BackupPhrase)
    , ipPort           :: !(Maybe NetworkAddress)
    , publicHost       :: !(Maybe String)
    , dhtKey           :: !(Maybe DHTKey)
    , timeLord         :: !Bool
    , jlPath           :: !(Maybe FilePath)
    , kademliaDumpPath :: !FilePath
    , webPort          :: !Word16
    , commonArgs       :: !CLI.CommonArgs
    , noSystemStart    :: !Int
    , monitorPort      :: !(Maybe Int)
    , notifierPort     :: !Word16
    }
  deriving Show
    
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

    ipPort <- optional $ CLI.externalNetworkAddressOption (Just ("0.0.0.0", 0))

    publicHost <- optional $ strOption $
        long "pubhost" <>
        metavar "HOST" <>
        help "Public host if different from one in --listen"

    dhtKey <- optional dhtKeyOption

    timeLord <- CLI.timeLordOption

    jlPath <- CLI.optionalJSONPath

    kademliaDumpPath <- strOption $
        long    "kademlia-dump-path" <>
        metavar "FILEPATH" <>
        value   "kademlia.dump" <>
        help    "Path to Kademlia dump file. If file doesn't exist, it will be created." <>
        showDefault

    webPort <- CLI.webPortOption 8080 "Port for web API."

    commonArgs <- CLI.commonArgsParser

    noSystemStart <- option auto (long "system-start" <> metavar "TIMESTAMP" <> value (-1))

    monitorPort <- optional $ option auto $
        long    "monitor-port" <>
        metavar "INT" <>
        help    "Run web monitor on this port."

    notifierPort <- option auto
         (long "notifier-port" <> metavar "PORT" <>
          value 8110 <> showDefault <>
          help "Port for update notifier")

    pure Args{..}


getExplorerOptions :: IO Args
getExplorerOptions = do
    (res, ()) <-
        simpleOptions
            ("cardano-explorer-" <> showVersion version)
            "CardanoSL explorer node"
            "CardanoSL explorer node."
            argsParser
            empty
    return res
