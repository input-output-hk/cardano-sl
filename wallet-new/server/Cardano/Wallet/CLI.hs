
module Cardano.Wallet.CLI where

import           Universum            hiding (show)

import           Data.Version         (showVersion)
import           Options.Applicative  (Parser, execParser, footerDoc, fullDesc, header,
                                       help, helper, info, infoOption, long, progDesc)
import           Paths_cardano_sl     (version)
import           Pos.Client.CLI       (CommonNodeArgs (..))
import qualified Pos.Client.CLI       as CLI
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo, compileInfo)


-- | The options parsed from the CLI when
-- starting up this wallet node.
data WalletStartupOptions = WalletStartupOptions {
      wsoNodeArgs :: !CommonNodeArgs
    }

walletArgsParser :: Parser WalletStartupOptions
walletArgsParser = WalletStartupOptions <$> CLI.commonNodeArgsParser

getWalletNodeOptions :: HasCompileInfo => IO WalletStartupOptions
getWalletNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> walletArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node w/ wallet."
                 <> header "Cardano SL node."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-node-" <> showVersion version <>
         ", git revision " <> toString (ctiGitRevision compileInfo))
        (long "version" <> help "Show version.")
