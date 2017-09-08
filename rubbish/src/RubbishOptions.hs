{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of cardano-rubbish

module RubbishOptions
       ( RubbishOptions (..)
       , RubbishAction (..)
       , getRubbishOptions
       ) where

import           Universum

import           Data.Version                 (showVersion)
import           NeatInterpolation            (text)
import           Options.Applicative          (CommandFields, Mod, Parser, command,
                                               execParser, footerDoc, fullDesc, header,
                                               help, helper, info, infoOption, long,
                                               metavar, progDesc, subparser)
import           Pos.Communication            (NodeId)
import           Serokell.Util.OptParse       (strOption)
import           Text.PrettyPrint.ANSI.Leijen (Doc)

import           Paths_cardano_sl             (version)
import qualified Pos.Client.CLI               as CLI

----------------------------------------------------------------------------
-- Types
----------------------------------------------------------------------------

data RubbishOptions = RubbishOptions
    { roAction         :: !RubbishAction
    , roCommonNodeArgs :: !CLI.CommonNodeArgs  -- ^ Common CLI args for nodes
    , roPeers          :: ![NodeId]
    -- ^ Peers with which we want to communicate
    --   TODO: we also have topology, so it can be redundant.
    }

data RubbishAction
    = Repl
    | Cmd { cmd :: !Text }

----------------------------------------------------------------------------
-- Parse action
----------------------------------------------------------------------------

actionParser :: Parser RubbishAction
actionParser = subparser $ replParser <> cmdParser

replParser :: Mod CommandFields RubbishAction
replParser = command "repl" $ info (pure Repl) $
             progDesc "Run REPL in console to evaluate the commands."

cmdParser :: Mod CommandFields RubbishAction
cmdParser = command "cmd" $ info opts desc
  where opts = Cmd <$> strOption (long "commands"
                               <> metavar "CMD"
                               <> help "Commands to execute, comma-separated.")
        desc = progDesc "Execute a list of predefined commands."

----------------------------------------------------------------------------
-- Parse everything
----------------------------------------------------------------------------

rubbishOptionsParser :: Parser RubbishOptions
rubbishOptionsParser = do
    roAction <- actionParser
    roCommonNodeArgs <- CLI.commonNodeArgsParser
    roPeers <- many $ CLI.nodeIdOption "peer" "Address of a peer."
    pure RubbishOptions {..}

getRubbishOptions :: IO RubbishOptions
getRubbishOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> rubbishOptionsParser) $
        fullDesc <> progDesc "Cardano SL CLI utilities."
                 <> header "CLI-based utilities (rubbish)."
                 <> footerDoc usageExample

    versionOption = infoOption
        ("cardano-rubbish-" <> showVersion version)
        (long "version" <> help "Show version.")

usageExample :: Maybe Doc
usageExample = (Just . fromString @Doc . toString @Text) [text|
Command example:

  stack exec -- cardano-rubbish                                  \
    --db-path node-db0                                           \
    --rebuild-db                                                 \
    --json-log=/tmp/logs/2017-05-22_181224/node0.json            \
    --logs-prefix /tmp/logs/2017-05-22_181224                    \
    --log-config /tmp/logs/2017-05-22_181224/conf/node0.log.yaml \
    --system-start 1495462345                                    \
    --peer 127.0.0.1:3001                                        \
    repl|]
