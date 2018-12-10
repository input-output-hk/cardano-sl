module Cardano.Wallet.Client.HttpsSettings
  ( mkHttpsManagerSettings
  , mkHttpManagerSettings
  , ServerId
  , AuthenticateServer(..)
  ) where

import           Data.Default (def)
import           Data.X509 (CertificateChain, SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Extra (validateDefaultWithIP)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (ManagerSettings, defaultManagerSettings)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientHooks (..), ClientParams (..),
                     Credential, Credentials (..), HostName, PrivKey, Shared (..),
                     Supported (..), noSessionManager)
import           Network.TLS.Extra.Cipher (ciphersuite_default)
import           Universum hiding (init)

-- | http-client supports service names
type ServerId = (HostName, Port)

-- | Port number or service name as string
type Port = ByteString

-- | Whether to check the server's SSL certificate
data AuthenticateServer = AuthenticateServer | AllowInsecure deriving (Show, Eq)

mkHttpManagerSettings :: ManagerSettings
mkHttpManagerSettings =
    defaultManagerSettings

-- | Sets up http client connection manager settings for optional
-- client authentication and/or server authentication.
mkHttpsManagerSettings
    :: ServerId                      -- ^ Target server hostname & port
    -> [SignedCertificate]           -- ^ CA certificate chain
    -> Maybe Credential              -- ^ Client credentials
    -> AuthenticateServer            -- ^ Whether to check server certificate against CA chain
    -> ManagerSettings
mkHttpsManagerSettings serverId caChain credentials authServer =
    mkManagerSettings tlsSettings sockSettings
  where
    sockSettings = Nothing
    tlsSettings  = TLSSettings clientParams
    clientParams = ClientParams
        { clientUseMaxFragmentLength    = Nothing
        , clientServerIdentification    = serverId
        , clientUseServerNameIndication = True
        , clientWantSessionResume       = Nothing
        , clientShared                  = clientShared
        , clientHooks                   = clientHooks
        , clientSupported               = clientSupported
        , clientDebug                   = def
        }
    clientShared = Shared
        { sharedCredentials     = Credentials $ maybeToList credentials
        , sharedCAStore         = makeCertificateStore caChain
        , sharedSessionManager  = noSessionManager
        , sharedValidationCache = def
        }
    clientHooks = def
        { onCertificateRequest = const . return $ credentials
        , onServerCertificate  = case authServer of
            AuthenticateServer -> validateDefaultWithIP
            AllowInsecure      -> \_ _ _ _ -> pure []
        }
    clientSupported = def
        { supportedCiphers = ciphersuite_default
        }
