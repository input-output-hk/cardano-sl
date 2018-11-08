{-# LANGUAGE LambdaCase #-}
module SetupTestEnv where
import           Universum

import           Cardano.Wallet.Client.Http
import           CLI
import qualified Data.ByteString.Char8 as B8
import           Data.X509.File (readSignedObject)
import           Network.HTTP.Client (Manager)

setupClient :: CLIOptions -> IO (WalletClient IO, Manager)
setupClient CLIOptions {..} = do
    let serverId = (serverHost, B8.pack $ show serverPort)
    caChain <- readSignedObject tlsCACertPath
    clientCredentials <- credentialLoadX509 tlsClientCertPath tlsPrivKeyPath >>= \case
        Right   a -> return a
        Left  err -> fail $ "Error decoding X509 certificates: " <> err
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials

    let
        baseUrl = BaseUrl Https serverHost serverPort mempty
        walletClient :: MonadIO m => WalletClient m
        walletClient = withThrottlingRetry
            . liftClient
            $ mkHttpClient baseUrl manager

    return (walletClient, manager)

