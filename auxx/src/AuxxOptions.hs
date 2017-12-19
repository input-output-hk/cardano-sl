{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of cardano-auxx

module AuxxOptions
       ( AuxxOptions (..)
       , AuxxAction (..)
       , AuxxStartMode (..)
       , getAuxxOptions
       ) where

import           Universum

import           Data.Version (showVersion)
import qualified NeatInterpolation as N
import           Options.Applicative (CommandFields, Mod, Parser, command, execParser, footerDoc,
                                      fullDesc, header, help, helper, info, infoOption, long,
                                      maybeReader, metavar, option, progDesc, subparser, value)
import           Pos.Communication (NodeId)
import           Serokell.Util.OptParse (strOption)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl (version)
import qualified Pos.Client.CLI as CLI
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo, compileInfo)

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data AuxxOptions = AuxxOptions
    { aoAction         :: !AuxxAction
    , aoCommonNodeArgs :: !CLI.CommonNodeArgs  -- ^ Common CLI args for nodes
    , aoPeers          :: ![NodeId]
    -- ^ Peers with which we want to communicate
    --   TODO: we also have topology, so it can be redundant.
    , aoStartMode      :: !AuxxStartMode
    }

data AuxxStartMode
    = Automatic
    | Light
    | WithConfiguration
    | WithNode
    deriving Eq

data AuxxAction
    = Repl
    | Cmd { cmd :: !Text }

----------------------------------------------------------------------------
-- Parse action
----------------------------------------------------------------------------

actionParser :: Parser AuxxAction
actionParser = subparser $ replParser <> cmdParser

replParser :: Mod CommandFields AuxxAction
replParser = command "repl" $ info (pure Repl) $
             progDesc "Run REPL in console to evaluate the commands."

cmdParser :: Mod CommandFields AuxxAction
cmdParser = command "cmd" $ info opts desc
  where opts = Cmd <$> strOption (long "commands"
                               <> metavar "CMD"
                               <> help "Commands to execute, comma-separated.")
        desc = progDesc "Execute a list of predefined commands."

----------------------------------------------------------------------------
-- Parse everything
----------------------------------------------------------------------------

helpContent :: String
helpContent = toString @Text $ [N.text|
    Auxx start mode. Can be 'light', 'with-config', 'with-node', 'auto'.
    * 'light' mode requires no configuration to start, but the available.
      command set is limited.
    * 'with-node' mode will start auxx as a node plugin.
    * 'with-config' mode will start auxx as a standalone application.
    * 'auto' will try to load the configuration and start in 'with-config' mode,
      if fails to do so will start in 'light' mode. (default: auto)
    |]

startModeParser :: Parser AuxxStartMode
startModeParser =
    option startModeReader $
    long "mode" <> value Automatic <> help helpContent
  where
    startModeReader =
        maybeReader $ \case
            "auto" -> Just Automatic
            "light" -> Just $ Light
            "with-config" -> Just $ WithConfiguration
            "with-node" -> Just $ WithNode
            _ -> Nothing


auxxOptionsParser :: Parser AuxxOptions
auxxOptionsParser = do
    aoAction <- actionParser
    aoCommonNodeArgs <- CLI.commonNodeArgsParser
    aoPeers <- many $ CLI.nodeIdOption "peer" "Address of a peer."
    aoStartMode <- startModeParser
    pure AuxxOptions {..}

getAuxxOptions :: HasCompileInfo => IO AuxxOptions
getAuxxOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> auxxOptionsParser) $
        fullDesc <> progDesc "Cardano SL CLI utilities."
                 <> header "CLI-based utilities (auxx)."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-auxx-" <> showVersion version <>
         ", git revision " <> toString (ctiGitRevision compileInfo))
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [N.text|
Command example:

  stack exec -- cardano-auxx                                     \
    --db-path run/auxx-db                                        \
    --rebuild-db                                                 \
    --json-log=/tmp/logs/2017-05-22_181224/node0.json            \
    --logs-prefix /tmp/logs/2017-05-22_181224                    \
    --log-config /tmp/logs/2017-05-22_181224/conf/node0.log.yaml \
    --system-start 1495462345                                    \
    --peer 127.0.0.1:3001                                        \
    repl|]
