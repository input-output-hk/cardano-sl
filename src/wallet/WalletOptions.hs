{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of cardano-wallet

module WalletOptions
       ( WalletOptions (..)
       , WalletAction (..)
       , getWalletOptions
       ) where

import           Data.String.QQ
import           Data.Version                 (showVersion)
import           Text.PrettyPrint.ANSI.Leijen (Doc)
import           Options.Applicative          (CommandFields, Mod, Parser, auto,
                                               command, execParser, footerDoc, fullDesc, help, header,
                                               helper, info, infoOption, long, metavar, option, option, progDesc,
                                               subparser, switch, value)
import           Serokell.Util.OptParse       (strOption)
import           Universum

import           Paths_cardano_sl             (version)
import qualified Pos.CLI                      as CLI
import           Pos.Communication            (NodeId)

data WalletOptions = WalletOptions
    { woDbPath      :: !FilePath
    , woRebuildDb   :: !Bool
    , woKeyFilePath :: !FilePath       -- ^ Path to file with secret keys
    , woDebug       :: !Bool           -- ^ Run in debug mode (with genesis keys included)
    , woJLFile      :: !(Maybe FilePath)
    , woCommonArgs  :: !CLI.CommonArgs -- ^ Common CLI args, including initial DHT nodes
    , woAction      :: !WalletAction
    , woPeers       :: ![NodeId]
    }

data WalletAction = Repl
                  | Cmd { cmd :: !Text }
#ifdef WITH_WEB
                  | Serve { webPort           :: !Word16
                          , webDaedalusDbPath :: !FilePath
                          }
#endif

actionParser :: Parser WalletAction
actionParser = subparser $ replParser <> cmdParser
#ifdef WITH_WEB
                        <> serveParser
#endif

replParser :: Mod CommandFields WalletAction
replParser = command "repl" $ info (pure Repl) $
             progDesc "Run REPL in console to evaluate the commands."

cmdParser :: Mod CommandFields WalletAction
cmdParser = command "cmd" $ info opts desc
  where opts = Cmd <$> strOption (long "commands"
                               <> metavar "CMD"
                               <> help "Commands to execute, comma-separated.")
        desc = progDesc "Execute a list of predefined commands."

#ifdef WITH_WEB
serveParser :: Mod CommandFields WalletAction
serveParser = command "serve" $ info opts desc
  where opts = Serve <$> CLI.webPortOption 8090    -- to differ from node's default port
                                           "Port for web server."
                     <*> option auto (long "daedalus-db-path"
                                   <> metavar "FILEPATH"
                                   <> value "run/daedalus-db"
                                   <> help "Path to the wallet database.")
        desc = progDesc "Serve HTTP Daedalus API on given port."
#endif

argsParser :: Parser WalletOptions
argsParser = do
    woDbPath <- strOption $
        long    "db-path" <>
        metavar "FILEPATH" <>
        value   "wallet-db" <>
        help    "Path to the wallet database."
    woRebuildDb <- switch $
        long "rebuild-db" <>
        help "If the DB already exist, discard its contents and \
             \create new one from scratch."
    woKeyFilePath <- strOption $
        long    "keys-path" <>
        metavar "FILEPATH" <>
        value   "secret.key" <>
        help    "Path to file with secret keys"
    woDebug <- switch $
        long "debug" <>
        help "Run in debug mode (with genesis keys included)"
    woJLFile <-
        CLI.optionalJSONPath
    woCommonArgs <-
        CLI.commonArgsParser
    woAction <-
        actionParser

    woPeers <- many $ CLI.nodeIdOption "peer" "Address of a peer."

    pure WalletOptions{..}

getWalletOptions :: IO WalletOptions
getWalletOptions = execParser programInfo >>= return
  where
    programInfo = info (helper <*> versionOption <*> argsParser) $
        fullDesc <> progDesc "Cardano SL CLI-wallet."
                 <> header "CLI-based wallet + node."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-wallet-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = Just [s|
Command example:

  stack exec -- cardano-wallet                                   \
    --db-path node-db0                                           \
    --rebuild-db                                                 \
    --json-log=/tmp/logs/2017-05-22_181224/node0.json            \
    --logs-prefix /tmp/logs/2017-05-22_181224                    \
    --log-config /tmp/logs/2017-05-22_181224/conf/node0.log.yaml \
    --system-start 1495462345                                    \
    --peer-id UJqMkyR7xplAn9fQdMo=                               \
    repl|]
