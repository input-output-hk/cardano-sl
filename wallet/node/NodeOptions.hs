{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP           #-}
{-# LANGUAGE QuasiQuotes   #-}

-- | Command line options of wallet node.

module NodeOptions
       ( WalletNodeArgs (..)
       , WalletArgs (..)
       , getWalletNodeOptions
       ) where

import           Data.Time.Units     (Minute)
import           Data.Version        (showVersion)
import           Options.Applicative (Parser, auto, execParser, footerDoc, fullDesc,
                                      header, help, helper, info, infoOption, long,
                                      metavar, option, progDesc, strOption, switch, value)
import qualified Options.Applicative as Opt
import           Universum           hiding (show)

import           Paths_cardano_sl    (version)
import           Pos.Client.CLI      (CommonNodeArgs (..))
import qualified Pos.Client.CLI      as CLI
import           Pos.Util.TimeWarp   (NetworkAddress, localhost)
import           Pos.Web.Types       (TlsParams (..))

data WalletNodeArgs = WalletNodeArgs CommonNodeArgs WalletArgs

data WalletArgs = WalletArgs
    { enableWeb          :: !Bool
    , webPort            :: !Word16
    , walletTLSParams    :: !TlsParams
    , walletAddress      :: !NetworkAddress
    , walletDbPath       :: !FilePath
    , walletRebuildDb    :: !Bool
    , walletAcidInterval :: !Minute
    , walletDebug        :: !Bool
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
    walletAcidInterval <- fmap fromInteger $ option auto $
        long "wallet-acid-cleanup-interval" <>
        help "Interval on which to execute wallet cleanup action (create checkpoint \
             \and archive and cleanup archive partially)" <>
        metavar "MINUTES" <>
        value (12 * 60)
    walletDebug <- switch $
        long "wallet-debug" <>
        help "Run wallet with debug params (e.g. include \
             \all the genesis keys in the set of secret keys)."

    pure $ WalletNodeArgs commonNodeArgs WalletArgs{..}

getWalletNodeOptions :: IO WalletNodeArgs
getWalletNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> walletArgsParser) $
        fullDesc <> progDesc "Cardano SL main server node w/ wallet."
                 <> header "Cardano SL node."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-node-" <> showVersion version)
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
