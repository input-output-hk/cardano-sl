{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase    #-}

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

import           Cardano.Wallet.API.V1.Types (ForceNtpCheck (..), NodeInfo (..),
                     SyncPercentage, SyncProgress (..), SyncThroughput (..),
                     mkSyncPercentage)
import           Universum

import           Cardano.Wallet.Client (ClientError (..), NewWallet, Resp,
                     ServantError (..), WalletClient (..), WalletId,
                     WalletResponse (..))
import           Cardano.Wallet.Client.Http (mkHttpClient, newManager)

import           Cardano.Wallet.API.V1.Types (SyncState (..), Wallet (..))
import           Cardano.Wallet.Client (getWallets)
import qualified Data.ByteString.Char8 as B8
import           Data.PEM (PEM (..), pemParseBS)
import           Data.X509 (SignedCertificate, decodeSignedCertificate)
import           Formatting (int, sformat, shown, (%))
import           Network.TLS (Credential, credentialLoadX509)
import qualified Pos.Core as Core
import           Pos.Node.API (BlockchainHeight (..), SyncPercentage (..))
import           Pos.Util.UnitsOfMeasure (MeasuredIn (..))
import           Servant.Client (BaseUrl (..), Scheme (Https))
import           System.FilePath (FilePath, (</>))

import           Cardano.Wallet.Client.HttpsSettings
import           Cardano.Wallet.Client.Wait

data ConnectConfig = ConnectConfig
  { cfgClientAuth         :: Maybe ClientAuthConfig
  , cfgCACertFile         :: Maybe FilePath
  , cfgAuthenticateServer :: AuthenticateServer
  , cfgBaseUrl            :: BaseUrl
  } deriving (Show)

data ClientAuthConfig = ClientAuthConfig
  { cfgCertFile    :: FilePath
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
    Nothing         -> pure []
  mcred <- case cfgClientAuth cfg of
    Just auth -> either throwM (pure . Just) =<< loadClientAuth auth
    Nothing   -> pure Nothing
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
