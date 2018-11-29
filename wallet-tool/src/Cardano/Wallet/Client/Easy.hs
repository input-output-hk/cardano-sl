{-# LANGUAGE LambdaCase, DeriveGeneric #-}

module Cardano.Wallet.Client.Easy
  ( -- * Setting up a WalletClient
    ConnectConfig(..)
  , AuthenticateServer(..)
  , ClientAuthConfig(..)
  , normalConnectConfig
  , walletClientFromConfig

  -- * Higher level functions
  , waitForSync
  , waitForRestore
  , SyncResult(..)
  , SyncError(..)
  , WaitOptions(..)
  , waitOptionsPID

  -- * Util
  , keyFromPEM, certFromPEM

  -- * Reexports
  , module Cardano.Wallet.Client
  , module Cardano.Wallet.API.V1.Types
  ) where

import           Universum
import           Cardano.Wallet.API.V1.Types (ForceNtpCheck (..), NodeInfo (..),
                     SyncPercentage, mkSyncPercentage, SyncProgress(..), SyncThroughput(..))

import           Cardano.Wallet.Client (ClientError (..), ServantError (..),
                     WalletClient (..), WalletResponse (..), Resp, NewWallet, WalletId)
import Cardano.Wallet.Client.Http (mkHttpClient, newManager)

import Pos.Node.API (SyncPercentage(..), BlockchainHeight(..))
import           Pos.Util.UnitsOfMeasure (MeasuredIn(..))
import           Control.Retry
import Network.TLS (Credential, credentialLoadX509)
import Data.X509 (SignedCertificate, decodeSignedCertificate)
import qualified Data.ByteString.Char8 as B8
import Servant.Client (BaseUrl(..), Scheme(Https))
import qualified Pos.Core as Core
import Formatting (sformat, shown, stext, int, (%), bprint, fixed)
import Data.PEM (PEM(..), pemParseBS)
import Data.Aeson (ToJSON(..), Value(..), (.=), object)
import Data.Default
import System.FilePath (FilePath, (</>))
import qualified Data.DList as DList
import           Formatting.Buildable (Buildable (..))
import Criterion.Measurement (initializeTime, getTime)
import System.Posix.Signals
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async (Async(..), cancel, withAsync, waitEitherCatchCancel)
import Control.Concurrent.STM.TVar (newTVar, readTVar, modifyTVar')
import Cardano.Wallet.Client (getWallets)
import Cardano.Wallet.API.V1.Types (SyncState(..), Wallet(..))

import Cardano.Wallet.ProcessUtil
import Cardano.Wallet.Client.HttpsSettings

data ConnectConfig = ConnectConfig
  { cfgClientAuth :: Maybe ClientAuthConfig
  , cfgCACertFile :: Maybe FilePath
  , cfgAuthenticateServer :: AuthenticateServer
  , cfgBaseUrl :: BaseUrl
  } deriving (Show)

data ClientAuthConfig = ClientAuthConfig
  { cfgCertFile :: FilePath
  , cfgPrivKeyFile :: FilePath
  } deriving (Show)

data ConfigError = ConfigCredentialsError Text
                 | ConfigCertificateError FilePath Text
                 deriving (Show, Typeable)

instance Exception ConfigError

-- | Conventional connect config for localhost, based on a wallet state path.
normalConnectConfig :: FilePath -- ^ Wallet state path -- contains 'tls' directory
                    -> Int -- ^ API listen port
                    -> ConnectConfig
normalConnectConfig stateDir port = ConnectConfig
  { cfgClientAuth = Just (ClientAuthConfig (tlsClient </> "client.crt") (tlsClient </> "client.key"))
  , cfgCACertFile = Just (tlsClient </> "ca.crt")
  , cfgAuthenticateServer = AuthenticateServer
  , cfgBaseUrl = BaseUrl Https "localhost" port ""
  }
  where tlsClient = stateDir </> "tls" </> "client"

walletClientFromConfig :: ConnectConfig -> IO (WalletClient IO)
walletClientFromConfig cfg = do
  ca <- case cfgCACertFile cfg of
    Just caCertFile -> either throwM return =<< loadCACert caCertFile
    Nothing -> pure []
  mcred <- case cfgClientAuth cfg of
    Just auth -> either throwM (pure . Just) =<< loadClientAuth auth
    Nothing -> pure Nothing
  let settings = httpsManagerSettings (cfgAuthenticateServer cfg) mcred ca (cfgServerId cfg)
  manager <- newManager settings
  pure $ mkHttpClient (cfgBaseUrl cfg) manager

loadClientAuth :: ClientAuthConfig -> IO (Either ConfigError Credential)
loadClientAuth ClientAuthConfig{..} = first errmsg <$> load
  where
    load = credentialLoadX509 cfgCertFile cfgPrivKeyFile
    errmsg = ConfigCredentialsError . sformat ("Couldn't decode certificates: "%shown)

loadCACert :: FilePath -> IO (Either ConfigError [SignedCertificate])
loadCACert f = parse <$> B8.readFile f
  where
    parse certText = case pemParseBS certText of
      Right pem -> case certFromPEM pem of
        Just bs -> case decodeSignedCertificate bs of
          Right sc -> Right [sc]
          Left err -> Left (ConfigCertificateError f $ sformat ("Couldn't decode certificate: "%shown) err)
        Nothing -> Left (ConfigCertificateError f "Could not find certificate in PEM file")
      Left err -> Left (ConfigCertificateError f $ sformat ("could not parse as pem file: "%shown) err)

-- | Get http-client connection tuple from 'ConnectConfig'.
cfgServerId :: ConnectConfig -> ServerId
cfgServerId = hostPort . cfgBaseUrl
  where hostPort b = (baseUrlHost b, B8.pack $ show $ baseUrlPort b)

-- | Gets the private key or certificate sections out of a parsed PEM file.
keyFromPEM, certFromPEM :: [PEM] -> Maybe ByteString
keyFromPEM = pemContentByName "RSA PRIVATE KEY"
certFromPEM = pemContentByName "CERTIFICATE"

-- | Finds a section by name in a parsed PEM file.
pemContentByName :: String -> [PEM] -> Maybe ByteString
pemContentByName name = fmap pemContent . find ((== name) . pemName)

data SyncResult r = SyncResult
  { syncResultError :: Maybe SyncError
  , syncResultDuration :: Double
  , syncResultData :: [(Double, r)]
  } deriving (Show, Typeable, Generic)

data SyncError = SyncErrorClient ClientError
               | SyncErrorProcessDied ProcessID
               | SyncErrorTimedOut Double
               | SyncErrorException SomeException
               | SyncErrorInterrupted
               deriving (Show, Typeable, Generic)

instance Buildable SyncError where
  build (SyncErrorClient err) = bprint ("There was an error connecting to the wallet: "%shown) err
  build (SyncErrorProcessDied pid) = bprint ("The cardano-node process with pid "%shown%" has gone") pid
  build (SyncErrorTimedOut t) = bprint ("Timed out after "%fixed 1%" seconds") t
  build (SyncErrorException e) = build e
  build SyncErrorInterrupted = build ("Interrupted" :: Text)

instance ToJSON r => ToJSON (SyncResult r) where
  toJSON (SyncResult err dur rs) =
    object $ ["data" .= toJSON rs, "duration" .= dur] <> status err
    where
      status Nothing = [ "success" .= True ]
      status (Just e) = [ "success" .= False, "error" .= String (show e) ]

instance ToJSON SyncError where
  toJSON e = String (show e)

-- | Unwrap sync percentage number
unSyncPercentage :: SyncPercentage -> Int
unSyncPercentage (SyncPercentage (MeasuredIn w)) = fromIntegral w

-- | Unwrap number of blocks
unBlockchainHeight :: BlockchainHeight -> Int
unBlockchainHeight (BlockchainHeight (MeasuredIn w)) = fromIntegral w

-- | Unwrap blocks per second
unSyncThroughput :: SyncThroughput -> Int
unSyncThroughput (SyncThroughput (MeasuredIn (Core.BlockCount w))) = fromIntegral w

waitForSync :: WaitOptions -> WalletClient IO -> IO (SyncResult (Int, Maybe Int))
waitForSync = waitForSomething (flip getNodeInfo NoNtpCheck) (pure . check)
  where
    check resp = (unfinished, msg, info)
      where
        progress = nfoSyncProgress resp
        unfinished = progress < mkSyncPercentage 100
        msg = sformat ("Blockchain syncing: "%int%"% "%int%"/"%int%" blocks")
              (unSyncPercentage progress)
              (fst info) (fromMaybe 0 (snd info))
        info = ( unBlockchainHeight $ nfoLocalBlockchainHeight resp
               , unBlockchainHeight <$> nfoBlockchainHeight resp )

waitForRestore :: WaitOptions -> WalletClient IO -> IO (SyncResult (Int, Int))
waitForRestore = waitForSomething getWallets (pure . check)
  where
    check resp = case [ (unSyncPercentage $ spPercentage sp, unSyncThroughput $ spThroughput sp)
                      | Restoring sp <- map walSyncState resp] of
                   ((pc, tp):_) -> (True, restoreMsg pc tp, (pc, tp))
                   [] -> (False, "Wallet restoration complete.", (100, 0))
    restoreMsg = sformat ("Wallet restoring: "%int%"% ("%int%" blocks/s)")

-- | Really basic timing information.
time :: IO a -> IO (Double, a)
time act = do
  initializeTime
  start <- getTime
  res <- act
  finish <- getTime
  pure (finish - start, res)


data WaitOptions = WaitOptions
  { waitTimeoutSeconds :: Maybe Double  -- ^ Timeout in seconds
  , waitProcessID :: Maybe ProcessID -- ^ Wallet process ID, so that crashes are handled
  , waitIntervalSeconds :: Double -- ^ Time between polls
  } deriving (Show, Eq)

instance Default WaitOptions where
  def = WaitOptions Nothing Nothing 1.0

waitOptionsPID :: Maybe ProcessID -> WaitOptions
waitOptionsPID pid = def { waitProcessID = pid }

waitForSomething :: (WalletClient IO -> Resp IO a) -- ^ Action to run on wallet
                 -> (a -> IO (Bool, Text, r)) -- ^ Action to interpret wallet response
                 -> WaitOptions
                 -> WalletClient IO -- ^ Wallet client
                 -> IO (SyncResult r)
waitForSomething req check WaitOptions{..} wc = do
  rs <- atomically $ newTVar DList.empty
  (dur, res) <- time $ withAsync (timeoutSleep waitTimeoutSeconds) $ \sleep -> do
    withAsync (retrying policy (check' rs) action) $ \poll -> cancelOnExit poll $
      waitEitherCatchCancel sleep poll

  rs' <- atomically $ readTVar rs

  let e = case res of
            Left _ -> SyncErrorTimedOut <$> waitTimeoutSeconds
            Right (Left err) -> Just (SyncErrorException err)
            Right (Right (True, _)) -> (SyncErrorProcessDied <$> waitProcessID)
            Right (Right (_, Left err)) -> (Just (SyncErrorClient err))
            _ -> Nothing

  pure $ SyncResult e dur (DList.toList rs')

  where
    policy = constantDelay (toMicroseconds waitIntervalSeconds)

    -- Run the given action and test that the server is still running
    action _st = (,) <$> checkProcessExists waitProcessID <*> req wc

    -- Interpret result of action, log some info, decide whether to continue
    check' rs st (_, Right resp) = do
      (unfinished, msg, res) <- check (wrData resp)
      let elapsed = fromIntegral (rsCumulativeDelay st) / oneSec
      atomically $ modifyTVar' rs (flip DList.snoc (elapsed, res))
      when unfinished $
        putStrLn $ sformat (fixed 2%" "%stext) elapsed msg
      pure unfinished
    check' _ _st (False, Left _) = do
      putStrLn $ sformat "Wallet is no longer running"
      pure False
    check' _ _st (True, Left err) = do
      putStrLn $ sformat ("Error connecting to wallet: "%shown) err
      pure True

timeoutSleep :: Maybe Double -> IO ()
timeoutSleep (Just s) = threadDelay (toMicroseconds s)
timeoutSleep Nothing = forever $ threadDelay (toMicroseconds 1000)

-- | Convert seconds to microseconds
toMicroseconds :: Double -> Int
toMicroseconds = floor . (* oneSec)

oneSec :: Num a => a
oneSec = 1000000

cancelOnExit :: Async a -> IO b -> IO b
cancelOnExit a b = cancelOnCtrlC a (cancelOnTerm a b)
  where
    addHandler sig h = installHandler sig h Nothing
    cancelOnSig sig as act = bracket (addHandler sig (Catch (cancel as))) (addHandler sig) (const act)
    cancelOnCtrlC = cancelOnSig keyboardSignal
    cancelOnTerm = cancelOnSig softwareTermination
