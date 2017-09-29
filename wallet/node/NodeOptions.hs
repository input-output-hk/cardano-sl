{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of wallet node.

module NodeOptions
       ( WalletNodeArgs (..)
       , WalletArgs (..)
       , getWalletNodeOptions
       ) where

import           Data.Version         (showVersion)
import           Options.Applicative  (Parser, execParser, footerDoc, fullDesc, header,
                                       help, helper, info, infoOption, long, progDesc,
                                       strOption, switch, value)
import qualified Options.Applicative  as Opt
import           Universum            hiding (show)

import           Paths_cardano_sl     (version)
import           Pos.Client.CLI       (CommonNodeArgs (..))
import qualified Pos.Client.CLI       as CLI
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo, compileInfo)
import           Pos.Util.TimeWarp    (NetworkAddress, localhost)
import           Pos.Web.Types        (TlsParams (..))

data WalletNodeArgs = WalletNodeArgs CommonNodeArgs WalletArgs

data WalletArgs = WalletArgs
    { enableWeb       :: !Bool
    , webPort         :: !Word16
    , walletTLSParams :: !TlsParams
    , walletAddress   :: !NetworkAddress
    , walletDbPath    :: !FilePath
    , walletRebuildDb :: !Bool
    , walletDebug     :: !Bool
    } deriving Show

walletArgsParser :: Parser WalletNodeArgs
walletArgsParser = do
    commonNodeArgs <- CLI.commonNodeArgsParser
    enableWeb <- switch $
        long "web" <>
        help "Activate web API (it’s not linked with a wallet web API)."
    webPort <-
        CLI.webPortOption 8080 "Port for web API."
    walletTLSParams <- tlsParamsOption
    walletAddress <- CLI.walletAddressOption $ Just (localhost, 8090)
    walletDbPath <- strOption $
        long  "wallet-db-path" <>
        help  "Path to the wallet's database." <>
        value "wallet-db"
    walletRebuildDb <- switch $
        long "wallet-rebuild-db" <>
        help "If wallet's database already exists, discard its contents \
             \and create a new one from scratch."
    walletDebug <- switch $
        long "wallet-debug" <>
        help "Run wallet with debug params (e.g. include \
             \all the genesis keys in the set of secret keys)."

    pure $ WalletNodeArgs commonNodeArgs WalletArgs{..}

getWalletNodeOptions :: HasCompileInfo => IO WalletNodeArgs
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

tlsParamsOption :: Opt.Parser TlsParams
tlsParamsOption = do
    tpCertPath <-
        Opt.strOption $
            CLI.templateParser
                "tlscert"
                "FILEPATH"
                "Path to file with TLS certificate"
                <> Opt.value "server.crt"
    tpKeyPath <-
        Opt.strOption $
            CLI.templateParser
                "tlskey"
                "FILEPATH"
                "Path to file with TLS key"
                <> Opt.value "server.key"
    tpCaPath <-
        Opt.strOption $
            CLI.templateParser
                "tlsca"
                "FILEPATH"
                "Path to file with TLS certificate authority"
                <> Opt.value "ca.crt"
    return TlsParams{..}
