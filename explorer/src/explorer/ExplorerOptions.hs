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

import           Paths_cardano_sl_explorer  (version)
import qualified Pos.Client.CLI             as CLI
import           Pos.Network.CLI            (NetworkConfigOpts, networkConfigOption)
import           Pos.Statistics             (EkgParams, StatsdParams, ekgParamsOption,
                                             statsdParamsOption)
import           Pos.Util.BackupPhrase      (BackupPhrase, backupPhraseWordsNum)

data Args = Args
    { dbPath            :: !FilePath
    , rebuildDB         :: !Bool
    , keyfilePath       :: !FilePath
    , backupPhrase      :: !(Maybe BackupPhrase)
    , networkConfigOpts :: !NetworkConfigOpts
      -- ^ Network configuration
      -- TODO: Does this obsolete 'peers' and 'peersFile'?
    , jlPath            :: !(Maybe FilePath)
    , webPort           :: !Word16
    , commonArgs        :: !CLI.CommonArgs
    , noSystemStart     :: !Int
    , noNTP             :: !Bool
    , enableMetrics     :: !Bool
    , ekgParams         :: !(Maybe EkgParams)
    , statsdParams      :: !(Maybe StatsdParams)
    , notifierPort      :: !Word16
    , staticPeers       :: !Bool
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

    networkConfigOpts <- networkConfigOption

    jlPath <- CLI.optionalJSONPath

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

    staticPeers <- switch $
        long "static-peers" <>
        help "Don't use Kademlia, use only static peers"

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
    pure res
