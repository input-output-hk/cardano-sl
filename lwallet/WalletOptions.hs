{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of cardano-wallet

module WalletOptions
       ( WalletOptions (..)
       , WalletAction (..)
       , getWalletOptions
       ) where

import           Universum

import           Data.Version                 (showVersion)
import           NeatInterpolation            (text)
import           Options.Applicative          (CommandFields, Mod, Parser, auto, command,
                                               execParser, footerDoc, fullDesc, header,
                                               help, helper, info, infoOption, long,
                                               metavar, option, progDesc, subparser,
                                               switch, value)
import           Serokell.Util.OptParse       (strOption)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl             (version)
import qualified Pos.Client.CLI               as CLI
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

actionParser :: Parser WalletAction
actionParser = subparser $ replParser <> cmdParser

replParser :: Mod CommandFields WalletAction
replParser = command "repl" $ info (pure Repl) $
             progDesc "Run REPL in console to evaluate the commands."

cmdParser :: Mod CommandFields WalletAction
cmdParser = command "cmd" $ info opts desc
  where opts = Cmd <$> strOption (long "commands"
                               <> metavar "CMD"
                               <> help "Commands to execute, comma-separated.")
        desc = progDesc "Execute a list of predefined commands."

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
getWalletOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> argsParser) $
        fullDesc <> progDesc "Cardano SL CLI-wallet."
                 <> header "CLI-based wallet + node."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-wallet-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [text|
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
