{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Command line options of pos-node.

module ExplorerOptions
       ( Args (..)
       , getExplorerOptions
       ) where

import           Data.Version               (showVersion)
import           Options.Applicative.Simple (Parser, auto, help, long, metavar, option,
                                             showDefault, simpleOptions, strOption,
                                             switch, value)
import           Prelude                    (show)
import           Serokell.Util.OptParse     (fromParsec)
import           Universum                  hiding (show)

import           Paths_cardano_sl_explorer  (version)
import qualified Pos.CLI                    as CLI
import           Pos.DHT.Model              (DHTKey)
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
    
-- FIXME: A monadid parser would be more understandable - https://github.com/input-output-hk/cardano-sl/blob/master/src/node/NodeOptions.hs#L64
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
    optional CLI.networkAddressOption <*>
    (optional $ strOption $
        long "pubhost" <>
        metavar "HOST" <>
        help "Public host if different from one in --listen") <*>
    optional
        (option (fromParsec CLI.dhtKeyParser) $
         long "dht-key" <> metavar "HOST_ID" <> help "DHT key in base64-url") <*>
    CLI.timeLordOption <*>
    CLI.optionalJSONPath <*>
    strOption
        (long "kademlia-dump-path" <> metavar "FILEPATH" <> showDefault <>
        help "Path to kademlia dump file" <> value "kademlia.dump")
    <*> CLI.webPortOption 8100 "Port for Explorer API"
    <*> CLI.commonArgsParser peerHelpMsg
    <*> option auto (long "system-start" <> metavar "TIMESTAMP" <> value (-1))
    <*> optional
            (option
                 auto
                 (long "monitor-port" <> metavar "INT" <>
                  help "Run web monitor on this port"))
    <*> option auto
         (long "notifier-port" <> metavar "PORT" <>
          value 8110 <> showDefault <>
          help "Port for update notifier")
  where
    peerHelpMsg =
        "Peer to connect to for initial peer discovery. Format\
        \ example: \"localhost:1234/MHdtsP-oPf7UWly7QuXnLK5RDB8=\""

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
