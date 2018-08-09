{-# LANGUAGE LambdaCase #-}
module SetupTestEnv where

import           Universum

import           Cardano.Wallet.Client.Http
import           CLI
import qualified Data.ByteString.Char8 as B8
import           Data.X509.File (readSignedObject)
import           Network.HTTP.Client (Manager)
import           System.IO (hSetEncoding, stdout, utf8)

setupClients
      :: IO ((WalletClient IO, Manager)
            ,(WalletClient IO, Manager)
            ,(WalletClient IO, Manager)
            ,(WalletClient IO, Manager))
setupClients = do
    hSetEncoding stdout utf8
    options@CLOptions {..} <- getOptions
    (,,,)
        <$> setupClient options serverPort0
        <*> setupClient options serverPort1
        <*> setupClient options serverPort2
        <*> setupClient options serverPort3

setupClient :: CLOptions -> Int -> IO (WalletClient IO, Manager)
setupClient CLOptions {..} port = do
    let serverId = (serverHost, B8.pack $ show port)
    caChain <- readSignedObject tlsCACertPath
    clientCredentials <- credentialLoadX509 tlsClientCertPath tlsPrivKeyPath >>= \case
        Right   a -> return a
        Left  err -> fail $ "Error decoding X509 certificates: " <> err
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials

    let baseUrl = BaseUrl Https serverHost port mempty
    return (mkHttpClient baseUrl manager, manager)
