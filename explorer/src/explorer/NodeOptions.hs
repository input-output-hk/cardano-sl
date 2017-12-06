{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of explorer node.

module NodeOptions
       ( ExplorerNodeArgs (..)
       , ExplorerArgs (..)
       , getExplorerNodeOptions
       ) where

import           Universum

import           Data.Version (showVersion)
import           Options.Applicative (Parser, auto, execParser, footerDoc, fullDesc, header, help,
                                      helper, info, infoOption, long, metavar, option, progDesc,
                                      showDefault, value)

import           Paths_cardano_sl_explorer (version)
import           Pos.Client.CLI (CommonNodeArgs (..))
import qualified Pos.Client.CLI as CLI


data ExplorerNodeArgs = ExplorerNodeArgs
    { enaCommonNodeArgs :: !CommonNodeArgs
    , enaExplorerArgs   :: !ExplorerArgs
    } deriving Show

-- | Explorer specific arguments.
data ExplorerArgs = ExplorerArgs
    { webPort      :: !Word16
    -- ^ The port for the explorer backend
    , notifierPort :: !Word16
    -- ^ The port for the socket.io backend
    } deriving Show

-- | Ther parser for the explorer arguments.
explorerArgsParser :: Parser ExplorerNodeArgs
explorerArgsParser = do
    commonNodeArgs <- CLI.commonNodeArgsParser
    webPort        <- CLI.webPortOption 8100 "Port for web API."
    notifierPort   <- option auto $
        long    "notifier-port" <>
        metavar "PORT" <>
        value   8110 <> showDefault <>
        help    "Port for update notifier, the socket.io backend."

    pure $ ExplorerNodeArgs commonNodeArgs ExplorerArgs{..}

-- | The parser for the explorer.
getExplorerNodeOptions :: IO ExplorerNodeArgs
getExplorerNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> explorerArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node w/ explorer."
                 <> header "Cardano SL explorer."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-explorer-" <> showVersion version)
        (long "version" <> help "Show version.")
