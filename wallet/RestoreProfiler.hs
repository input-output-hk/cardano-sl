{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE LambdaCase                 #-}

module Main (main) where

import Universum
import           Network.HTTP.Client (Manager, newManager)
import qualified Data.ByteString as BS
import           Network.TLS (ClientParams (clientHooks, clientSupported), credentialLoadX509FromMemory,
                     defaultParamsClient, onCertificateRequest,
                     onServerCertificate, supportedCiphers)
import           Servant.Client.Core (BaseUrl (BaseUrl), Scheme (Https))
import           Control.Lens hiding ((.=), (^.))
import           Cardano.Wallet.Client.Http (mkHttpClient, WalletClient, getNodeInfo, ForceNtpCheck(NoNtpCheck),
                      APIResponse(APIResponse), NodeInfo(nfoSyncProgress, NodeInfo), BackupPhrase(BackupPhrase),
                      postWallet, NewWallet(NewWallet), WalletOperation(RestoreWallet), AssuranceLevel(NormalAssurance),
                      SyncState(Restoring, Synced), spPercentage, spEstimatedCompletionTime, spThroughput,
                      ClientError, WalletId(WalletId), getWallet, Wallet(walSyncState))
import           Data.Aeson (FromJSON (parseJSON), withObject,
                     (.:), eitherDecode)
import           Data.Text.Lens (packed)
import           Data.Default (def)
import           Network.TLS.Extra.Cipher (ciphersuite_strong)
import           Network.Connection (TLSSettings (TLSSettings))
import           Network.HTTP.Client.TLS (mkManagerSettings)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import Pos.Util.Jsend (ResponseStatus(SuccessStatus))
import           Cardano.Wallet.API.Types.UnitOfMeasure (MeasuredIn (MeasuredIn))
import Pos.Node.API (SyncPercentage(SyncPercentage))
import Control.Concurrent(threadDelay)
import Cardano.Mnemonic (mkMnemonic, MnemonicError, Mnemonic)
import           Formatting (Format, fprint, later, shown, (%))
import Cardano.Wallet.API.V1.Types (EstimatedCompletionTime(EstimatedCompletionTime), SyncThroughput(SyncThroughput), Wallet(walId))
import Pos.Core (BlockCount(BlockCount))
import           Formatting.Internal.Raw (left)

data FaucetConfig = FaucetConfig {
    -- | Host the wallet API is running on
    _fcWalletApiHost       :: !String
    -- | Port the wallet API is running on
  , _fcWalletApiPort       :: !Int
    -- | TLS public certificate
  , _fcPubCertFile         :: !FilePath
    -- | TLS private key
  , _fcPrivKeyFile         :: !FilePath
  , _fcWalletPhrase        :: [Text]
  }

makeClassy ''FaucetConfig

instance FromJSON FaucetConfig where
    parseJSON = withObject "FaucetConfig" $ \v ->
        FaucetConfig
            <$> v .: "wallet-host"
            <*> v .: "wallet-port"
            <*> v .: "public-certificate"
            <*> v .: "private-key"
            <*> v .: "wallet-phrase"

main :: IO ()
main = do
  let
    getCfg :: [FilePath] -> IO FaucetConfig
    getCfg [ path ] = do
      ecfg <- eitherDecode <$> BSL.readFile path
      case ecfg of
        Left err -> error $ T.pack err
        Right cfg -> pure cfg
    getCfg _ = do
      error "usage: restore-profiler config.json"
  args <- getArgs
  fc <- getCfg args
  manager <- liftIO $ createManager fc
  let
    url = BaseUrl Https (fc ^. fcWalletApiHost) (fc ^. fcWalletApiPort) ""
    client = mkHttpClient url manager
  putStrLn ("hello world"::String)
  mainLoop client fc

mainLoop :: WalletClient IO -> FaucetConfig -> IO ()
mainLoop client fc = do
  nodeInfo <- getNodeInfo client NoNtpCheck
  case nodeInfo of
    Right (APIResponse (NodeInfo{nfoSyncProgress}) SuccessStatus _paging) -> do
      case nfoSyncProgress of
        SyncPercentage (MeasuredIn 100) -> do
          print ("synced"::String)
          restoreWallet client fc
        _ -> do
          print nfoSyncProgress
          threadDelay 1000000
          mainLoop client fc
    y -> do
      print y

restoreWallet :: WalletClient IO -> FaucetConfig -> IO ()
restoreWallet client fc = do
  let
    onError :: MnemonicError 4 -> IO BackupPhrase
    onError err = fail $ "Invalid BackupPhrase provided" <> show err
    onSuccess :: Mnemonic 12 -> IO BackupPhrase
    onSuccess = return . BackupPhrase
    phrase :: [Text] -> IO BackupPhrase
    phrase ws = either onError onSuccess (mkMnemonic ws)
  seed <- phrase (fc ^. fcWalletPhrase)
  print seed
  wallet <- successfulRequest $ postWallet client $ NewWallet seed Nothing NormalAssurance "profiling wallet" RestoreWallet
  print wallet
  checkProgress client fc (walId wallet)

checkProgress :: WalletClient IO -> FaucetConfig -> WalletId -> IO ()
checkProgress client fc wid = do
  progress <- successfulRequest $ getWallet client wid
  case (walSyncState progress) of
    Restoring syncProg -> do
      let
        EstimatedCompletionTime (MeasuredIn msRemaining) = spEstimatedCompletionTime syncProg
        SyncThroughput (MeasuredIn (BlockCount blocksPerSecond)) = spThroughput syncProg
        SyncPercentage (MeasuredIn percent) = spPercentage syncProg
        paddedShown :: Show a => Format r (a -> r)
        paddedShown = later f
          where
            f a = (left 10 ' ' (show a :: String))
      fprint ("remaining: " % paddedShown % ", rate: " % paddedShown % ", percent: " % shown % "\n") msRemaining blocksPerSecond percent
      threadDelay 10000000
      checkProgress client fc wid
    Synced -> do
      print ("done syncing wallet"::String)

-- | Makes a http client 'Manager' for communicating with the wallet node
createManager :: FaucetConfig -> IO Manager
createManager fc = do
    pubCert <- BS.readFile (fc ^. fcPubCertFile)
    privKey <- BS.readFile (fc ^. fcPrivKeyFile)
    case credentialLoadX509FromMemory pubCert privKey of
        Left problem -> error $ "Unable to load credentials: " <> (problem ^. packed)
        Right credential ->
            let hooks = def {
                            onCertificateRequest = \_ -> return $ Just credential,
                            -- Only connects to localhost so this isn't required
                            onServerCertificate  = \_ _ _ _ -> return []
                        }
                clientParams = (defaultParamsClient "localhost" "") {
                                   clientHooks = hooks,
                                   clientSupported = def {
                                       supportedCiphers = ciphersuite_strong
                                   }
                               }
                tlsSettings = TLSSettings clientParams
            in
            newManager $ mkManagerSettings tlsSettings Nothing

-- | Run a given request as above, but throws if it fails
successfulRequest
    :: Show a
    => IO (Either (ClientError) (APIResponse a))
    -> IO a
successfulRequest request = do
    reply <- request
    case reply of
        Left e  ->
            fail . ("expected a successful response but got an error: " <>) . show $ e
        Right (APIResponse a SuccessStatus _pages) ->
            return a
        Right x -> fail . ("unexpected failure " <>) . show $ x
