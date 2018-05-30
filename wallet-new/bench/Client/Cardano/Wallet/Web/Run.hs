-- | Run endpoint client.

module Client.Cardano.Wallet.Web.Run
    ( runEndpointClient
    ) where

import           Universum

import           Servant.Client             (ClientM, ClientEnv (..), runClientM)
import           Servant.Client.Core        (BaseUrl (..), Scheme (..))

import           Data.Default               (def)
import           Network.Connection         (TLSSettings (..))
import           Network.HTTP.Client        (Manager, newManager)
import           Network.HTTP.Client.TLS    (mkManagerSettings)
import           Network.TLS                (ClientParams (..), credentialLoadX509FromMemory,
                                             defaultParamsClient, onCertificateRequest,
                                             onServerCertificate, supportedCiphers)
import           Network.TLS.Extra.Cipher   (ciphersuite_all)

import           Bench.Cardano.Wallet.Types (CompleteConfig (..))

-- | Run client for particular endpoint. It is assumed that
-- node is already running, with enabled Wallet Web API.
runEndpointClient
    :: CompleteConfig
    -> ClientM a
    -> IO (Either Text a)
runEndpointClient CompleteConfig {..} realClient = do
    manager <- makeClientManager tlsPubCert tlsPrivKey
    runClientM realClient (ClientEnv manager nodeURL) >>= \case
        Left problem   -> return . Left  $ toText (show problem :: String)
        Right response -> return . Right $ response
  where
    nodeURL  = BaseUrl Https nodeHost nodePort ""
    nodeHost = "localhost"
    nodePort = 8090

-- | Load credential files, because node requires TLS-connection with certificate.
makeClientManager
    :: ByteString
    -> ByteString
    -> IO Manager
makeClientManager pubCert privKey =
    case credentialLoadX509FromMemory pubCert privKey of
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
