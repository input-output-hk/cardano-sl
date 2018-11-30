{-# LANGUAGE LambdaCase, ExistentialQuantification #-}

module Cardano.Wallet.Client.CLI
  where

import Universum
import           Options.Applicative
import qualified Data.Text as T
import Data.Bifunctor (first)
import Servant.Client (BaseUrl(..), Scheme(Https))
import Cardano.Mnemonic (mkMnemonic)
import Cardano.Wallet.API.V1.Types (WalletOperation(..), BackupPhrase(..), NewWallet(..), AssuranceLevel(..), V1(..), mkPassPhrase, WalletId(..), AccountIndex, mkAccountIndex)
import Cardano.Wallet.ProcessUtil (ProcessID)
import qualified Pos.Crypto.Signing as Core
import           Pos.Core.NetworkAddress (addrParserNoWildcard)
import qualified Data.ByteString.Char8 as B8
import qualified Text.Parsec as Parsec
import qualified Data.Text.IO as T
import System.Exit (ExitCode(..))
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Aeson (ToJSON(..))
import Formatting (sformat, shown, (%), string)
import Data.Aeson (encodeFile)
import Data.Aeson.Encode.Pretty (encodePretty)
import Criterion.Measurement (secs)

import Cardano.Wallet.Client.Easy

----------------------------------------------------------------------------
-- CLI Types

-- | The command from the user
data Action m = WaitForSync (Maybe ProcessID) (Maybe FilePath)
              | WaitForRestore (Maybe ProcessID) (Maybe FilePath)
              | forall a. ToJSON a => WalletEndpoint (WalletCall m a) -- ^ Wallet API call
              | WalletEndpointVoid (WalletCallVoid m) -- ^ Wallet API call with empty response

-- | An API call
type WalletCall m a = WalletClient m -> Resp m a

-- | An API call with no data in the response
type WalletCallVoid m = WalletClient m -> m (Either ClientError ())

----------------------------------------------------------------------------
-- Option parsers

optionsParser :: Parser (ConnectConfig, Action IO)
optionsParser = (,) <$> connectConfigP <*> actionP

connectConfigP :: Parser ConnectConfig
connectConfigP = ConnectConfig
  <$> optional (clientAuthCertKeyP <|> clientAuthPemP)
  <*> optional (strOption
                (long "cacert"
                 <> metavar "FILENAME"
                 <> help "CA certificate chain for authenticating the server"))
  <*> authenticateServerP
  <*> baseUrlP

clientAuthCertKeyP :: Parser ClientAuthConfig
clientAuthCertKeyP = ClientAuthConfig
  <$> strOption (long "cert"
                 <> metavar "FILENAME"
                 <> help "X509 certificate file")
  <*> strOption (long "key"
                 <> metavar "FILENAME"
                 <> help "Certificate key file")

clientAuthPemP :: Parser ClientAuthConfig
clientAuthPemP = pemConfig <$> strOption (long "pem"
                                          <> metavar "FILENAME"
                                          <> help "Combined X509 certificate and key file")
  where pemConfig pem = ClientAuthConfig pem pem

baseUrlP :: Parser BaseUrl
baseUrlP = baseUrl <$> addrP <*> path
  where
    baseUrl (host, port) = BaseUrl Https (B8.unpack host) (fromIntegral port)
    addrP = argument readAddrM (metavar "HOST:PORT" <> value ("localhost", 8090) <> help "Wallet API host and port to connect to")
    readAddrM = eitherReader (first show . Parsec.parse addrParserNoWildcard "" . T.pack)
    -- host = argument str (metavar "HOST" <> value "localhost" <> help "Wallet API host to connect to")
    -- port = argument auto (metavar "PORT" <> value 8090 <> help "Wallet API port to connect to")
    path = strOption (long "path" <> short 'p' <> value "" <> metavar "PATH" <> help "Base URL path")

authenticateServerP :: Parser AuthenticateServer
authenticateServerP = flag AllowInsecure AuthenticateServer
                      ( long "insecure"
                      <> short 'k'
                      <> help "Skip server certificate authentication" )

----------------------------------------------------------------------------
-- Action parsers

actionP :: Parser (Action m)
actionP = hsubparser
  ( command "wait-for-sync" (info waitForSyncP (progDesc "Poll wallet until it has fully synced its chain"))
    <> command "wait-for-restore" (info waitForRestoreP (progDesc "Poll wallet until the restore operation is complete"))
    <> commandGroup "High-level commands"
  )
  <|> apiActionP

waitForSyncP :: Parser (Action m)
waitForSyncP = WaitForSync <$> optional pidP <*> optional outfileP

waitForRestoreP :: Parser (Action m)
waitForRestoreP = WaitForRestore <$> optional pidP <*> optional outfileP

----------------------------------------------------------------------------
-- API action parsers

apiActionP :: Parser (Action m)
apiActionP = hsubparser (mconcat [ command name (info p (progDesc desc))
                                 | (name, desc, p) <- commands]
                         <> commandGroup "Basic API calls"
                         <> hidden)
  where
    commands =
      [ ("node-info", "Query node info", WalletEndpoint <$> nodeInfoP)
      , ("create-wallet", "Create a new wallet from mnemonic", WalletEndpoint <$> createWalletP CreateWallet)
      , ("restore-wallet", "Restore a wallet from mnemonic", WalletEndpoint <$> createWalletP RestoreWallet)
      , ("delete-wallet", "Delete a wallet", WalletEndpointVoid <$> deleteWalletP)
      , ("delete-account", "Delete account", WalletEndpointVoid <$> deleteAccountP)
      -- fixme: more endpoints, less boilerplate
      ]

nodeInfoP :: Parser (WalletCall m NodeInfo)
nodeInfoP = (\ntp wc -> getNodeInfo wc ntp) <$> ntpCheck
  where
    ntpCheck = flag NoNtpCheck ForceNtpCheck (long "force-ntp-check")

createWalletP :: WalletOperation -> Parser (WalletCall m Wallet)
createWalletP op = (\nw wc -> postWallet wc nw) <$> newWalletP
  where
    newWalletP = NewWallet
                 <$> backupPhraseP
                 <*> optional spendingPasswordP
                 <*> assuranceLevelP
                 <*> nameP
                 <*> pure op
    backupPhraseP = option parseBackupPhrase (long "backup-phrase" <> metavar "WORDS" <> help "12-word mnemonic")
    spendingPasswordP = option parsePassPhrase (long "spending-password" <> metavar "PASSWORD" <> help "32-byte hex-encoded passphrase")
    assuranceLevelP = flag NormalAssurance StrictAssurance
                      ( long "strict-assurance"
                        <> help "Assurance level strict" )
    nameP = T.pack <$> strOption (long "name" <> metavar "NAME" <> value "New Wallet" <> help "Name for the wallet")

deleteWalletP :: Parser (WalletCallVoid m)
deleteWalletP = (\wid wc -> deleteWallet wc wid) <$> walletIdP

deleteAccountP :: Parser (WalletCallVoid m)
deleteAccountP = (\wid accIdx wc -> deleteAccount wc wid accIdx) <$> walletIdP <*> accountIndexP


----------------------------------------------------------------------------
-- Little option parsers

pidP :: Parser ProcessID
pidP = option auto (long "pid" <> metavar "PID" <> help "PID of cardano-node")

outfileP :: Parser FilePath
outfileP = strOption (long "out" <> short 'o' <> metavar "FILE" <> help "Output JSON timing info")

parseBackupPhrase :: ReadM BackupPhrase
parseBackupPhrase = eitherReader (first show . fmap BackupPhrase . mkMnemonic . T.words . T.pack)

parsePassPhrase :: ReadM (V1 Core.PassPhrase)
parsePassPhrase = eitherReader (first show . fmap V1 . mkPassPhrase . T.pack)

walletIdP :: Parser WalletId
walletIdP = WalletId . T.pack <$> argument str (metavar "HASH" <> help "Wallet ID")

accountIndexP :: Parser AccountIndex
accountIndexP = argument (eitherReader accIndex) (metavar "INTEGER" <> help "Account index")
  where
    accIndex s = case readMaybe s of
                   Just idx -> first show (mkAccountIndex idx)
                   Nothing -> Left "Account index is not a number"


----------------------------------------------------------------------------
-- Program

runAction :: Action IO -> WalletClient IO -> IO ExitCode
runAction act wc = case act of
  WaitForSync mpid out -> waitForSync (waitOptionsPID mpid) wc >>= handleWaitResult out
  WaitForRestore mpid out -> waitForRestore (waitOptionsPID mpid) wc >>= handleWaitResult out
  WalletEndpoint req -> req wc >>= printStatus
  WalletEndpointVoid req -> req wc >>= printStatusVoid

printStatus :: ToJSON a => Either ClientError a -> IO ExitCode
printStatus = printStatus' . fmap Just

printStatusVoid :: Either ClientError () -> IO ExitCode
printStatusVoid = printStatus' . fmap (const (Nothing :: Maybe ()))

printStatus' :: ToJSON a => Either ClientError (Maybe a) -> IO ExitCode
printStatus' resp = case resp of
  Right Nothing -> pure ExitSuccess
  Right (Just a) -> L8.putStrLn (encodePretty a) >> pure ExitSuccess
  Left cerr -> (T.hPutStrLn stderr $ sformat ("client error: "%shown) cerr) >> pure (ExitFailure 100)

handleWaitResult :: ToJSON r => Maybe FilePath -> SyncResult r -> IO ExitCode
handleWaitResult mout res@(SyncResult err start dur _) = do
  putStrLn (msg err)
  putStrLn $ sformat ("Started: "%shown) start
  putStrLn $ sformat ("Elapsed time: "%string) (secs dur)
  whenJust mout (writeJSON res)
  pure (code err)
  where
    msg :: Maybe SyncError -> Text
    msg = maybe "Finished" show

    code Nothing = ExitSuccess
    code (Just (SyncErrorClient _)) = ExitFailure 1
    code (Just (SyncErrorProcessDied _)) = ExitFailure 2
    code (Just (SyncErrorTimedOut _)) = ExitFailure 3
    code (Just (SyncErrorException _)) = ExitFailure 4
    code (Just (SyncErrorInterrupted)) = ExitSuccess

    writeJSON sr f = do
      putStrLn $ sformat ("Writing output to "%shown) f
      encodeFile f sr
