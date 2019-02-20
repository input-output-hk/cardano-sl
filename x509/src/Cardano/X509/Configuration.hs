--
-- | Configuration for generating X509 certificates
--
-- Users are expected to provide configuration (from file or by hand) and use
-- 'fromConfiguration' to turn them into description.
-- Then, full description can be turned into certificates via 'genCertificate'
--
-- Configuration --fromConfiguration--> Description --genCertificate--> Certificate

module Cardano.X509.Configuration
    (
    -- * Configuration for Certificates
      TLSConfiguration(..)
    , ServerConfiguration(..)
    , CertConfiguration(..)
    , DirConfiguration(..)

    -- * Description of Certificates
    , CertDescription(..)

    -- * Effectful Functions
    , ConfigurationKey(..)
    , ErrInvalidTLSConfiguration
    , ErrInvalidExpiryDays
    , fromConfiguration
    , decodeConfigFile
    , genCertificate
    ) where

import           Universum

import           Control.Monad ((>=>))
import           Crypto.PubKey.RSA (PrivateKey, PublicKey)
import           Data.ASN1.OID (OIDable (..))
import           Data.Hourglass (Minutes (..), Period (..), dateAddPeriod,
                     timeAdd)
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import           Data.X509 (DistinguishedName (..), DnElement (..),
                     ExtAuthorityKeyId (..), ExtBasicConstraints (..),
                     ExtExtendedKeyUsage (..), ExtKeyUsage (..),
                     ExtKeyUsageFlag (..), ExtKeyUsagePurpose (..),
                     ExtSubjectAltName (..), ExtSubjectKeyId (..),
                     ExtensionRaw, Extensions (..), PubKey (..),
                     SignedCertificate, extensionEncode, hashDN)
import           Data.X509.Validation (ValidationChecks (..), defaultChecks)
import           Data.Yaml (decodeFileEither, parseMonad, withObject)
import           GHC.Generics (Generic)
import           System.IO (FilePath)
import           Time.System (dateCurrent)
import           Time.Types (DateTime (..))

import           Data.X509.Extra (parseSAN, signAlgRSA256, signCertificate)

import           Data.Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.HashMap.Lazy as HM
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.X509 as X509


--
-- Configuration of Certificates
--

-- | Foreign Configuration, pulled from within a .yaml file
data TLSConfiguration = TLSConfiguration
    { tlsCa      :: CertConfiguration
    , tlsServer  :: ServerConfiguration
    , tlsClients :: [CertConfiguration]
    } deriving (Generic, Show, Eq)

instance FromJSON TLSConfiguration where
    parseJSON = withObject "TLSConfiguration" $ \o -> TLSConfiguration <$> o .: "ca" <*> o .: "server" <*> o .: "clients"

instance ToJSON TLSConfiguration where
    toJSON conf = object
      [ "ca"      .= tlsCa conf
      , "server"  .= tlsServer conf
      , "clients" .= tlsClients conf
      ]

-- | Output directories configuration
data DirConfiguration = DirConfiguration
    { outDirServer  :: FilePath
    , outDirClients :: FilePath
    , outDirCA      :: Maybe FilePath
    } deriving (Generic, Show, Eq)

-- | Foreign Certificate Configuration
data CertConfiguration = CertConfiguration
    { certOrganization :: String
    , certCommonName   :: String
    , certExpiryDays   :: Int
    } deriving (Generic, Show, Eq)

instance FromJSON CertConfiguration where
    parseJSON = withObject "CertConfiguration" $ \o -> CertConfiguration <$> o .: "organization" <*> o .: "commonName" <*> o .: "expiryDays"

instance ToJSON CertConfiguration where
    toJSON certconf = object
      [ "organization" .= certOrganization certconf
      , "commonName"   .= certCommonName certconf
      , "expiryDays"   .= certExpiryDays certconf
      ]

-- | Foreign Server Certificate Configuration (SANS extra options)
data ServerConfiguration = ServerConfiguration
    { serverConfiguration :: CertConfiguration
    , serverAltNames      :: NonEmpty String
    } deriving (Generic, Show, Eq)

-- NOTE We keep the declaration structure 'flat' such that servers config
-- are simply client config with an extra field 'altDNS'
instance FromJSON ServerConfiguration where
    parseJSON v = ServerConfiguration
        <$> ((withObject "CertConfiguration" $ \o -> CertConfiguration <$> o .: "organization" <*> o .: "commonName" <*> o .: "expiryDays") v)
        <*> ((withObject "ServerConfiguration" $ \o -> o .: "altDNS") v)

instance ToJSON ServerConfiguration where
    toJSON conf = object
      [ "organization" .= (certOrganization $ serverConfiguration conf)
      , "commonName" .= (certCommonName $ serverConfiguration conf)
      , "expiryDays" .= (certExpiryDays $ serverConfiguration conf)
      , "altDNS" .= serverAltNames conf
      ]

--
-- Description of Certificates
--

-- | Internal full-representation of a certificate
data CertDescription m pub priv outdir = CertDescription
    { certConfiguration :: CertConfiguration
    , certSerial        :: Int
    , certExtensions    :: [ExtensionRaw]
    , certIssuer        :: DistinguishedName
    , certSubject       :: DistinguishedName
    , certGenKeys       :: m (pub, priv)
    , certSigningKey    :: priv
    , certOutDir        :: outdir
    , certFilename      :: String
    , certChecks        :: ValidationChecks
    }


--
-- Effectful Functions
--


-- | Type-alias for signature readability
newtype ConfigurationKey = ConfigurationKey
    { getConfigurationKey :: String
    } deriving (Eq, Show)


newtype ErrInvalidExpiryDays
    = ErrInvalidExpiryDays String
    deriving (Show)

instance Exception ErrInvalidExpiryDays


newtype ErrInvalidTLSConfiguration
    = ErrInvalidTLSConfiguration String
    deriving (Show)

instance Exception ErrInvalidTLSConfiguration


-- | Describe a list of certificates to generate & sign from a foreign config
--
-- Description can then be used with @genCertificate@ to obtain corresponding
-- certificate
fromConfiguration
    :: (Applicative m)
    => TLSConfiguration -- ^ Foreign TLS configuration / setup
    -> DirConfiguration -- ^ Output directories configuration
    -> m (pub, priv)    -- ^ Key pair generator
    -> (pub, priv)      -- ^ Initial / Root key pair
    -> (CertDescription m pub priv (Maybe String), [CertDescription m pub priv String])
    -- ^ PKI description matching provided conf, fst = CA, snd = server & clients
fromConfiguration tlsConf dirConf genKeys (caPub, caPriv) =
    let
        caDN = mkDistinguishedName (tlsCa tlsConf)

        caConfig = CertDescription
            { certConfiguration = tlsCa tlsConf
            , certSerial        = 1
            , certExtensions    = caExtensionsV3 caDN
            , certIssuer        = caDN
            , certSubject       = caDN
            , certGenKeys       = pure (caPub, caPriv)
            , certSigningKey    = caPriv
            , certOutDir        = outDirCA dirConf
            , certFilename      = "ca"
            , certChecks        = defaultChecks
            }

        ServerConfiguration tlsServer' serverAltDNS = tlsServer tlsConf
        svDN = mkDistinguishedName tlsServer'
        svConfig = CertDescription
            { certConfiguration = tlsServer'
            , certSerial        = 2
            , certExtensions    = svExtensionsV3 svDN caDN serverAltDNS
            , certIssuer        = caDN
            , certSubject       = svDN
            , certGenKeys       = genKeys
            , certSigningKey    = caPriv
            , certOutDir        = outDirServer dirConf
            , certFilename      = "server"
            , certChecks        = defaultChecks
            }

        clConfigs = forEach (tlsClients tlsConf) $ \(i, tlsClient) ->
            let
                clDN = mkDistinguishedName tlsClient
                suffix = if i == 0 then "" else "_" <> show i
            in CertDescription
                { certConfiguration = tlsClient
                , certSerial        = 3 + i
                , certExtensions    = clExtensionsV3 clDN caDN
                , certIssuer        = caDN
                , certSubject       = clDN
                , certGenKeys       = genKeys
                , certSigningKey    = caPriv
                , certOutDir        = outDirClients dirConf
                , certFilename      = "client" <> suffix
                , certChecks        = defaultChecks { checkFQHN = False }
                }
    in
        (caConfig, svConfig : clConfigs)


-- | Decode a configuration file (.yaml). The expected file structure is:
--     <configuration-key>:
--       tls:
--         ca: ...
--         server: ...
--         clients: ...
--
-- where the 'configuration-key' represents the target environment (dev, test,
-- bench, etc.).
decodeConfigFile
    :: (MonadIO m, MonadThrow m)
    => ConfigurationKey -- ^ Target configuration Key
    -> FilePath         -- ^ Target configuration file
    -> m TLSConfiguration
decodeConfigFile (ConfigurationKey cKey) filepath =
    decodeFileMonad filepath >>= parseMonad parser
  where
    errMsg key = "property '"<> key <> "' " <> "not found in configuration file."

    decodeFileMonad = (liftIO . decodeFileEither) >=> either
        (throwM . ErrInvalidTLSConfiguration . show)
        return

    parser = withObject "TLS Configuration" (parseK cKey >=> parseK "tls")

    parseK :: FromJSON a => String -> Aeson.Object -> Aeson.Parser a
    parseK key = maybe (fail $ errMsg key) parseJSON . HM.lookup (toText key)


-- | Generate & sign a certificate from a certificate description
genCertificate
    :: CertDescription IO PublicKey PrivateKey filename
    -> IO (PrivateKey, SignedCertificate)
genCertificate desc = do
    ((pub, priv), now) <- (,) <$> (certGenKeys desc) <*> dateCurrent

    let conf = certConfiguration desc

    when (certExpiryDays conf <= 0) $
        throwM $ ErrInvalidExpiryDays "expiry days should be a positive integer"

    let cert = X509.Certificate
            { X509.certVersion      = 2
            , X509.certSerial       = fromIntegral (certSerial desc)
            , X509.certSignatureAlg = signAlgRSA256
            , X509.certValidity     = (addMinutes (-1) now, addDays (certExpiryDays conf) now)
            , X509.certPubKey       = PubKeyRSA pub
            , X509.certExtensions   = Extensions (Just $ certExtensions desc)
            , X509.certIssuerDN     = certIssuer desc
            , X509.certSubjectDN    = certSubject desc
            }

    (priv,) <$> signCertificate (certSigningKey desc) cert
  where
    addDays :: Int -> DateTime -> DateTime
    addDays n time@(DateTime date _) =
        time { dtDate = dateAddPeriod date (mempty { periodDays = n }) }

    addMinutes :: Int -> DateTime -> DateTime
    addMinutes n time =
        timeAdd time (Minutes $ fromIntegral n)


--
-- Internals
--

caExtensionsV3 :: DistinguishedName -> [ExtensionRaw]
caExtensionsV3 dn =
    let
        keyUsage         = ExtKeyUsage [KeyUsage_keyCertSign, KeyUsage_cRLSign]
        basicConstraints = ExtBasicConstraints True (Just 0)
        subjectKeyId     = ExtSubjectKeyId (hashDN dn)
        authorityKeyId   = ExtAuthorityKeyId (hashDN dn)
    in
        [ extensionEncode True keyUsage
        , extensionEncode True basicConstraints
        , extensionEncode False subjectKeyId
        , extensionEncode False authorityKeyId
        ]


usExtensionsV3 :: ExtKeyUsagePurpose -> DistinguishedName -> DistinguishedName -> [ExtensionRaw]
usExtensionsV3 purpose subDN issDN =
    let
        keyUsage         = ExtKeyUsage [KeyUsage_digitalSignature, KeyUsage_keyEncipherment]
        basicConstraints = ExtBasicConstraints False Nothing
        subjectKeyId     = ExtSubjectKeyId (hashDN subDN)
        authorityKeyId   = ExtAuthorityKeyId (hashDN issDN)
        extendedKeyUsage = ExtExtendedKeyUsage [purpose]
    in
        [ extensionEncode True keyUsage
        , extensionEncode False extendedKeyUsage
        , extensionEncode False basicConstraints
        , extensionEncode False subjectKeyId
        , extensionEncode False authorityKeyId
        ]


svExtensionsV3 :: DistinguishedName -> DistinguishedName -> NonEmpty String -> [ExtensionRaw]
svExtensionsV3 subDN issDN altNames =
    let
        subjectAltName =
            ExtSubjectAltName $ map parseSAN (NonEmpty.toList altNames)
    in
        extensionEncode False subjectAltName :
        usExtensionsV3 KeyUsagePurpose_ServerAuth subDN issDN


clExtensionsV3 :: DistinguishedName -> DistinguishedName -> [ExtensionRaw]
clExtensionsV3 =
    usExtensionsV3 KeyUsagePurpose_ClientAuth

forEach :: [a] -> ((Int, a) -> b) -> [b]
forEach xs fn =
    zipWith (curry fn) [0..(length xs - 1)] xs


mkDistinguishedName :: CertConfiguration -> DistinguishedName
mkDistinguishedName conf = DistinguishedName
    [ (getObjectID DnOrganization, fromString $ certOrganization conf)
    , (getObjectID DnCommonName, fromString $ certCommonName conf)
    ]
