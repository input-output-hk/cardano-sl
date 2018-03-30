{- | The module which contains parsing facilities for
     the CLI options passed to this edge node.
-}
module Cardano.Wallet.Server.CLI where

import           Universum

import           Data.List (drop, span)
import           Data.Time.Units (Minute)
import           Data.Version (showVersion)
import           Options.Applicative (Parser, ReadM, auto, execParser, footerDoc, fullDesc, header,
                                      help, helper, info, infoOption, long, metavar, option,
                                      progDesc, str, strOption, switch, value)
import           Paths_cardano_sl (version)
import           Pos.Client.CLI (CommonNodeArgs (..))
import qualified Pos.Client.CLI as CLI
import           Pos.Util.CompileInfo (CompileTimeInfo (..), HasCompileInfo, compileInfo)
import           Pos.Util.TimeWarp (NetworkAddress, localhost)
import           Pos.Web (TlsParams (..))


-- | The options parsed from the CLI when starting up this wallet node.
-- This umbrella data type includes the node-specific options for this edge node
-- plus the wallet backend specific options.
data WalletStartupOptions = WalletStartupOptions {
      wsoNodeArgs            :: !CommonNodeArgs
    , wsoWalletBackendParams :: !ChooseWalletBackend
    }

-- | TODO: Once we get rid of the legacy wallet, remove this.
data ChooseWalletBackend =
    WalletLegacy !WalletBackendParams
  | WalletNew    !NewWalletBackendParams

-- | DB-specific options.
data WalletDBOptions = WalletDBOptions {
      walletDbPath       :: !FilePath
      -- ^ The path for the wallet-backend DB.
    , walletRebuildDb    :: !Bool
      -- ^ Whether or not to wipe and rebuild the DB.
    , walletAcidInterval :: !Minute
    , walletFlushDb      :: !Bool
    } deriving Show

-- | The startup parameters for the legacy wallet backend.
-- Named with the suffix `Params` to honour other types of
-- parameters like `NodeParams` or `SscParams`.
data WalletBackendParams = WalletBackendParams
    { enableMonitoringApi :: !Bool
    -- ^ Whether or not to run the monitoring API.
    , monitoringApiPort   :: !Word16
    -- ^ The port the monitoring API should listen to.
    , walletTLSParams     :: !(Maybe TlsParams)
    -- ^ The TLS parameters.
    , walletAddress       :: !NetworkAddress
    -- ^ The wallet address.
    , walletDocAddress    :: !NetworkAddress
    -- ^ The wallet documentation address.
    , walletRunMode       :: !RunMode
    -- ^ The mode this node is running in.
    , walletDbOptions     :: !WalletDBOptions
    -- ^ DB-specific options.
    } deriving Show

-- | Start up parameters for the new wallet backend
--
-- TODO: This just wraps the legacy parameters at the moment.
data NewWalletBackendParams = NewWalletBackendParams WalletBackendParams
    deriving (Show)

-- | A richer type to specify in which mode we are running this node.
data RunMode = ProductionMode
             -- ^ Run in production mode
             | DebugMode
             -- ^ Run in debug mode
             deriving Show

-- | Converts a @GenesisKeysInclusion@ into a @Bool@.
isDebugMode :: RunMode -> Bool
isDebugMode ProductionMode = False
isDebugMode DebugMode      = True

-- | Parses and returns the @WalletStartupOptions@ from the command line.
getWalletNodeOptions :: HasCompileInfo => IO WalletStartupOptions
getWalletNodeOptions = execParser programInfo
  where
    programInfo = info (helper <*> versionOption <*> walletStartupOptionsParser) $
        fullDesc <> progDesc "Cardano SL edge node w/ wallet."
                 <> header "Cardano SL edge node."
                 <> footerDoc CLI.usageExample

    versionOption = infoOption
        ("cardano-node-" <> showVersion version <>
         ", git revision " <> toString (ctiGitRevision compileInfo))
        (long "version" <> help "Show version.")

-- | The main @Parser@ for the @WalletStartupOptions@
walletStartupOptionsParser :: Parser WalletStartupOptions
walletStartupOptionsParser = WalletStartupOptions <$> CLI.commonNodeArgsParser
                                                  <*> chooseWalletBackendParser

-- | Choose between the two backends
--
-- TODO: This implementation of the parser is not very elegant. I'd prefer
-- something along the lines of
--
-- > chooseWalletBackendParser :: Parser ChooseWalletBackend
-- > chooseWalletBackendParser = asum [
-- >       WalletNew    <$> newWalletBackendParamsParser
-- >     , WalletLegacy <$> walletBackendParamsParser
-- >     ]
--
-- but for some reason this isn't working, perhaps because optparse-applicative
-- isn't backtracking enough? Dunno.
chooseWalletBackendParser :: Parser ChooseWalletBackend
chooseWalletBackendParser = choose
    <$> walletBackendParamsParser
    <*> (switch $ mconcat [
            long "new-wallet"
          , help "Use the new wallet implementation (NOT FOR PRODUCTION USE)"
          ])
  where
    choose opts False = WalletLegacy $ opts
    choose opts True  = WalletNew    $ NewWalletBackendParams opts

-- | The @Parser@ for the @WalletBackendParams@.
walletBackendParamsParser :: Parser WalletBackendParams
walletBackendParamsParser = WalletBackendParams <$> enableMonitoringApiParser
                                                <*> monitoringApiPortParser
                                                <*> tlsParamsParser
                                                <*> addressParser
                                                <*> docAddressParser
                                                <*> runModeParser
                                                <*> dbOptionsParser
  where
    enableMonitoringApiParser :: Parser Bool
    enableMonitoringApiParser = switch (long "monitoring-api" <>
                                        help "Activate the node monitoring API."
                                       )

    monitoringApiPortParser :: Parser Word16
    monitoringApiPortParser = CLI.webPortOption 8080 "Port for the monitoring API."

    addressParser :: Parser NetworkAddress
    addressParser = CLI.walletAddressOption $ Just (localhost, 8090)

    docAddressParser :: Parser NetworkAddress
    docAddressParser = CLI.docAddressOption $ Just (localhost, 8091)

    runModeParser :: Parser RunMode
    runModeParser = (\debugMode -> if debugMode then DebugMode else ProductionMode) <$>
        switch (long "wallet-debug" <>
                help "Run wallet with debug params (e.g. include \
                     \all the genesis keys in the set of secret keys)."
               )

tlsParamsParser :: Parser (Maybe TlsParams)
tlsParamsParser = constructTlsParams <$> certPathParser
                                     <*> keyPathParser
                                     <*> caPathParser
                                     <*> clientsParser
                                     <*> disabledParser
  where
    constructTlsParams tpCertPath tpKeyPath tpCaPath tpClients disabled =
        guard (not disabled) $> TlsParams{..}

    certPathParser :: Parser FilePath
    certPathParser = strOption (CLI.templateParser
                                "tlscert"
                                "FILEPATH"
                                "Path to file with TLS certificate"
                                <> value "scripts/tls-files/server.crt"
                               )

    keyPathParser :: Parser FilePath
    keyPathParser = strOption (CLI.templateParser
                               "tlskey"
                               "FILEPATH"
                               "Path to file with TLS key"
                               <> value "scripts/tls-files/server.key"
                              )

    caPathParser :: Parser FilePath
    caPathParser = strOption (CLI.templateParser
                              "tlsca"
                              "FILEPATH"
                              "Path to file with TLS certificate authority"
                              <> value "scripts/tls-files/ca.crt"
                             )

    clientsParser :: Parser [String]
    clientsParser = option strList (CLI.templateParser
                                    "tlsclients"
                                    "LIST"
                                    "A comma-separated list of accepted clients"
                                    <> value ["Daedalus Wallet"]
                                   )

    disabledParser :: Parser Bool
    disabledParser = switch $
                     long "no-tls" <>
                     help "Disable tls. If set, 'tlscert', 'tlskey' \
                          \and 'tlsca' options are ignored"

    strList :: ReadM [String]
    strList =
        str >>= failIfEmpty . splitOn ','
      where
        splitOn c s =
          case span (/= c) s of
            (h, []) -> [h]
            (h, q)  -> h : splitOn c (drop 1 q)

        failIfEmpty xs =
          case xs of
            [] -> empty
            _  -> pure xs


-- | The parser for the @WalletDBOptions@.
dbOptionsParser :: Parser WalletDBOptions
dbOptionsParser = WalletDBOptions <$> dbPathParser
                                  <*> rebuildDbParser
                                  <*> acidIntervalParser
                                  <*> flushDbParser
  where
    dbPathParser :: Parser FilePath
    dbPathParser = strOption (long  "wallet-db-path" <>
                              help  "Path to the wallet's database." <>
                              value "wallet-db"
                             )

    rebuildDbParser :: Parser Bool
    rebuildDbParser = switch (long "wallet-rebuild-db" <>
                              help "If wallet's database already exists, discard \
                                   \its contents and create a new one from scratch."
                             )

    acidIntervalParser :: Parser Minute
    acidIntervalParser = fromInteger <$>
        option auto (long "wallet-acid-cleanup-interval" <>
                     help "Interval on which to execute wallet cleanup \
                          \action (create checkpoint and archive and \
                          \cleanup archive partially)" <>
                     metavar "MINUTES" <>
                     value (12 * 60)
                    )

    flushDbParser :: Parser Bool
    flushDbParser = switch (long "flush-wallet-db" <>
                            help "Flushes all blockchain-recoverable data from DB \
                                 \(everything excluding wallets/accounts/addresses, \
                                 \metadata)"
                           )
