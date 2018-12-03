module Cardano.Wallet.Client.HttpsSettings
  ( mkHttpsManagerSettingsInsecure
  , httpsManagerSettings
  , ServerId
  , AuthenticateServer(..)
  ) where

import           Cardano.Wallet.Client.Http (Port, mkHttpsManagerSettings)
import           Data.Default (def)
import           Data.X509 (SignedCertificate)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (ManagerSettings)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientHooks (..), ClientParams (..), Credential,
                     HostName, Supported (..), defaultParamsClient)
import           Network.TLS.Extra.Cipher (ciphersuite_default)
import           Universum hiding (init)

-- | http-client supports service names
type ServerId = (HostName, Port)

-- | Whether to check the server's SSL certificate
data AuthenticateServer = AuthenticateServer | AllowInsecure deriving (Show, Eq)

-- | Sets up http client connection manager settings for client
-- authentication but without server authentication.
mkHttpsManagerSettingsInsecure :: ServerId -> Maybe Credential -> ManagerSettings
mkHttpsManagerSettingsInsecure (name, port) cred = mkManagerSettings tlsSettings Nothing
  where
    tlsSettings = TLSSettings clientParams
    hooks = def {
      onCertificateRequest = \_ -> pure cred,
      -- Ignore the server certificate
      onServerCertificate  = \_ _ _ _ -> pure []
      }
    clientParams = (defaultParamsClient name port) {
      clientHooks = hooks,
      clientSupported = def { supportedCiphers = ciphersuite_default }
      }

httpsManagerSettings :: AuthenticateServer -> Maybe Credential -> [SignedCertificate] -> ServerId -> ManagerSettings
httpsManagerSettings AuthenticateServer (Just cred) ca host = mkHttpsManagerSettings host ca cred
httpsManagerSettings AuthenticateServer Nothing _ _ = error "herp derp not implemented"
httpsManagerSettings AllowInsecure cred _ host = mkHttpsManagerSettingsInsecure host cred
