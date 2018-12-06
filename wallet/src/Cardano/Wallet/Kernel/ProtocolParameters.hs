{-# LANGUAGE LambdaCase #-}
module Cardano.Wallet.Kernel.ProtocolParameters where

import           Universum

import qualified Data.ByteString.Char8 as B8
import           Servant.Client (BaseUrl (..), Scheme (..))

import           Cardano.Node.Client (NodeClient (..), NodeHttpClient,
                     mkHttpClient)
import           Cardano.Node.Manager (credentialLoadX509)
import           Cardano.Wallet.Server.CLI
import           Network.HTTP.Client (Manager, newManager)
--import           Cardano.Node.API
import           Cardano.Node.Manager (mkHttpsManagerSettings, readSignedObject)
--import qualified Pos.Node.API as P
import           Pos.Util.Wlog (logInfo)
import           Pos.Web.Types

import           Pos.Node.API (ProtocolParameters (..))


import           Pos.Core.Slotting (SlotId (..))


data ProtocolParameterAdaptor = ProtocolParameterAdaptor
    {
      nodeClient   :: NodeHttpClient

    , getTipSlotId :: IO SlotId
    }

newProtocolParameterAdaptor :: NodeHttpClient -> ProtocolParameterAdaptor
newProtocolParameterAdaptor client = ProtocolParameterAdaptor
    { nodeClient    = client
    , getTipSlotId = f $ slotId <$> getProtocolParameters client
    }
      where
        f :: MonadIO m => Show e => Show a => ExceptT e m a -> m a
        f e = do
            x <- runExceptT e
            case x of
                Right a   -> liftIO $ logInfo (show a) >> return a
                Left  err -> error $ "ProtocolParameters:44" <> (show err)

setupClient :: NewWalletBackendParams -> IO (NodeHttpClient, Manager)
setupClient (NewWalletBackendParams params) = do
    let (serverHost', serverPort') = ("127.0.0.1", 8083 :: Int)
    let (serverHost, serverPort) = (B8.unpack serverHost', fromIntegral serverPort')
    let serverId = (serverHost, B8.pack $ show serverPort)


    let tlsParams = maybe (error "TODO") id $ walletTLSParams params
    logInfo $ show tlsParams
    let tlsPrivKeyPath    = "scripts/tls-files/client.pem"
    let tlsClientCertPath = "scripts/tls-files/client.crt"

    let tlsCACertPath = tpCaPath tlsParams


    logInfo $ "Localhost: " <> (show serverHost)
    logInfo $ "Priv key path: " <> (show tlsPrivKeyPath)

    caChain <- readSignedObject tlsCACertPath

    clientCredentials <- credentialLoadX509 tlsClientCertPath tlsPrivKeyPath >>= \case
        Right   a -> return a
        Left  err -> fail $ "Error decoding X509 certificates: " <> err
    manager <- newManager $ mkHttpsManagerSettings serverId caChain clientCredentials

    let
        baseUrl = BaseUrl Https serverHost serverPort mempty
        walletClient = mkHttpClient baseUrl manager

    return (walletClient, manager)
