-- | Run endpoint client.

module Client.Pos.Wallet.Web.Run
    ( runEndpointClient
    ) where

import           Universum

import           Servant.Client           (ClientM, ClientEnv (..), runClientM)
import           Servant.Common.BaseUrl   (BaseUrl (..), Scheme (..))

import           Data.Default             (def)
import           Network.Connection       (TLSSettings (..))
import           Network.HTTP.Client      (Manager, newManager)
import           Network.HTTP.Client.TLS  (mkManagerSettings)
import           Network.TLS              (ClientParams (..), credentialLoadX509,
                                           defaultParamsClient, onCertificateRequest,
                                           onServerCertificate, supportedCiphers)
import           Network.TLS.Extra.Cipher (ciphersuite_all)

-- | Run client for particular endpoint. It is assumed that
-- node is already running, with enabled Wallet Web API.
runEndpointClient :: ClientM a -> IO (Either Text a)
runEndpointClient realClient = do
    manager <- makeClientManager
    runClientM realClient (ClientEnv manager nodeURL) >>= \case
        Left problem   -> return . Left  $ toText (show problem :: String)
        Right response -> return . Right $ response
  where
    nodeURL  = BaseUrl Https nodeHost nodePort ""
    nodeHost = "localhost"
    nodePort = 8090

-- | Load credential files, because node requires TLS-connection with certificate.
makeClientManager :: IO Manager
makeClientManager = credentialLoadX509 pathToPubCert pathToPrivKey >>= \case
    Left problem -> error . toText $ "Unable to load credentials: " <> problem
    Right credential ->
        let hooks = def {
                        onCertificateRequest = \_ -> return $ Just credential,
                        onServerCertificate  = \_ _ _ _ -> return []
                    }
            clientParams = (defaultParamsClient "localhost" "") {
                               clientHooks = hooks,
                               clientSupported = def {
                                   supportedCiphers = ciphersuite_all
                               }
                           }
            tlsSettings = TLSSettings clientParams
        in
        newManager $ mkManagerSettings tlsSettings Nothing
  where
    -- TODO: Fix paths.
    pathToPubCert = "/home/denis/cardano-sl/scripts/tls-files/ca.crt"
    pathToPrivKey = "/home/denis/cardano-sl/scripts/tls-files/server.key"
