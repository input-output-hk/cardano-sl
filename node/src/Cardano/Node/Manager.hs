{-# LANGUAGE ScopedTypeVariables #-}

module Cardano.Node.Manager
    ( -- * Smart-constructors
      mkHttpManagerSettings
    , mkHttpsManagerSettings

    -- * Re-exports, helpers to load X509 certificates and private key
    , credentialLoadX509
    , readSignedObject
    , newManager
    , Manager
    ) where

import           Universum

import           Data.ByteString (ByteString)
import           Data.Default (Default (..))
import           Data.X509 (CertificateChain, SignedCertificate)
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Extra (validateDefaultWithIP)
import           Data.X509.File (readSignedObject)
import           Network.Connection (TLSSettings (..))
import           Network.HTTP.Client (Manager, ManagerSettings,
                     defaultManagerSettings, managerResponseTimeout,
                     newManager, responseTimeoutMicro)
import           Network.HTTP.Client.TLS (mkManagerSettings)
import           Network.TLS (ClientHooks (..), ClientParams (..),
                     Credentials (..), HostName, PrivKey, Shared (..),
                     Supported (..), credentialLoadX509, noSessionManager)
import           Network.TLS.Extra.Cipher (ciphersuite_default)

type Port = ByteString


mkHttpManagerSettings :: ManagerSettings
mkHttpManagerSettings =
    defaultManagerSettings


mkHttpsManagerSettings
    :: (HostName, Port)              -- ^ Target server hostname & port
    -> [SignedCertificate]           -- ^ CA certificate chain
    -> (CertificateChain, PrivKey)   -- ^ (Client certificate, Client key)
    -> ManagerSettings
mkHttpsManagerSettings serverId caChain credentials =
    (mkManagerSettings tlsSettings sockSettings)
        { managerResponseTimeout = responseTimeoutSeconds 60
        }
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
        { sharedCredentials     = Credentials [credentials]
        , sharedCAStore         = makeCertificateStore caChain
        , sharedSessionManager  = noSessionManager
        , sharedValidationCache = def
        }
    clientHooks = def
        { onCertificateRequest = const . return . Just $ credentials
        , onServerCertificate  = validateDefaultWithIP
        }
    clientSupported = def
        { supportedCiphers = ciphersuite_default
        }

    responseTimeoutSeconds a = responseTimeoutMicro (a * 1000 * 1000)
