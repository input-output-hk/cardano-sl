module Configuration
    ( decodeEitherConfigFile
    , fromConfiguration
    , ConfigurationKey
    , TLSConfiguration(..)
    , DirConfiguration(..)
    , ServerConfiguration(..)
    , CertConfiguration(..)
    , CertDescription(..)
    ) where

import           Control.Monad ((>=>))
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Aeson (FromJSON (..))
import           Data.ASN1.OID (OIDable (..))
import           Data.List (stripPrefix)
import           Data.Semigroup ((<>))
import           Data.String (fromString)
import           Data.X509
import           Data.Yaml (decodeFileEither, parseEither, withObject)
import           GHC.Generics (Generic)
import           System.IO (FilePath)
import           Universum (toText)

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.Char as Char
import qualified Data.HashMap.Lazy as HM


-- | Decode a configuration file (.yaml). The expected file structure is:
--     <configuration-key>:
--       tls:
--         ca: ...
--         server: ...
--         clients: ...
--
-- where the 'configuration-key' represents the target environment (dev, test,
-- bench, etc.).
decodeEitherConfigFile
    :: ConfigurationKey -- ^ Target configuration Key
    -> FilePath         -- ^ Target configuration file
    -> ExceptT String IO TLSConfiguration
decodeEitherConfigFile cKey filepath = ExceptT
    (either (Left . show) (parseEither parser) <$> decodeFileEither filepath)
  where
    parser = withObject "TLS Configuration" (parseK cKey >=> parseK "tls")

    parseK :: FromJSON a => String -> Aeson.Object -> Aeson.Parser a
    parseK key =
        let
            errMsg = "Invalid TLS Configuration: property '"<> key <> "' "
                <> "not found in configuration file."
        in
            maybe (fail errMsg) parseJSON . HM.lookup (toText key)


-- | Type-alias for signature readability
type ConfigurationKey = String

-- | Foreign Configuration, pulled from within a .yaml file
data TLSConfiguration = TLSConfiguration
    { tlsCa      :: CertConfiguration
    , tlsServer  :: ServerConfiguration
    , tlsClients :: [CertConfiguration]
    } deriving (Generic)

instance FromJSON TLSConfiguration where
    parseJSON = Aeson.genericParseJSON (aesonDropPrefix "tls")

-- | Output directories configuration
data DirConfiguration = DirConfiguration
    { outDirServer  :: FilePath
    , outDirClients :: FilePath
    , outDirCA      :: Maybe FilePath
    }

-- | Foreign Certificate Configuration
data CertConfiguration = CertConfiguration
    { certOrganization :: String
    , certCommonName   :: String
    , certExpiryDays   :: Int
    } deriving (Generic)

instance FromJSON CertConfiguration where
    parseJSON = Aeson.genericParseJSON (aesonDropPrefix "cert")

-- | Foreign Server Certificate Configuration (SANS extra options)
data ServerConfiguration = ServerConfiguration
    { serverConfiguration :: CertConfiguration
    , serverAltNames      :: [String]
    }
-- NOTE We keep the declaration structure 'flat' such that servers config
-- are simply client config with an extra field 'altDNS'
instance FromJSON ServerConfiguration where
    parseJSON v = ServerConfiguration
        <$> parseJSON v
        <*> Aeson.withObject "ServerConfiguration" parseDNS v
      where
        parseDNS =
            let
                errMsg = "Invalid Server Configuration: missing property 'altDNS'"
            in
                maybe (fail errMsg) parseJSON . HM.lookup "altDNS"


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
    }


-- | Describe a list of certificates to generate & sign from a foreign config
fromConfiguration
    :: Applicative m
    => TLSConfiguration -- ^ Foreign TLS configuration / setup
    -> DirConfiguration -- ^ Output directories configuration
    -> m (pub, priv)    -- ^ Key pair generator
    -> (pub, priv)      -- ^ Initial / Root key pair
    -> (CertDescription m pub priv (Maybe String), [CertDescription m pub priv String])
    -- ^ PKI description matching provided conf, fst = CA, snd = server & clients
fromConfiguration TLSConfiguration{..} DirConfiguration{..} certGenKeys (caPub, caPriv) =
    let
        caDN = mkDistinguishedName tlsCa

        caConfig = CertDescription
            { certConfiguration = tlsCa
            , certSerial        = 1
            , certExtensions    = caExtensionsV3 caDN
            , certIssuer        = caDN
            , certSubject       = caDN
            , certGenKeys       = pure (caPub, caPriv)
            , certSigningKey    = caPriv
            , certOutDir        = outDirCA
            , certFilename      = "ca"
            }

        ServerConfiguration tlsServer' serverAltDNS = tlsServer
        svDN = mkDistinguishedName tlsServer'
        svConfig = CertDescription
            { certConfiguration = tlsServer'
            , certSerial        = 2
            , certExtensions    = svExtensionsV3 svDN caDN serverAltDNS
            , certIssuer        = caDN
            , certSubject       = svDN
            , certGenKeys       = certGenKeys
            , certSigningKey    = caPriv
            , certOutDir        = outDirServer
            , certFilename      = "server"
            }

        clConfigs = forEach tlsClients $ \(i, tlsClient) ->
            let
                clDN = mkDistinguishedName tlsClient
                suffix = if i == 0 then "" else "_" <> show i
            in CertDescription
                { certConfiguration = tlsClient
                , certSerial        = 3 + i
                , certExtensions    = clExtensionsV3 clDN caDN
                , certIssuer        = caDN
                , certSubject       = clDN
                , certGenKeys       = certGenKeys
                , certSigningKey    = caPriv
                , certOutDir        = outDirClients
                , certFilename      = "client" <> suffix
                }
    in
        (caConfig, svConfig : clConfigs)
  where
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

    svExtensionsV3 :: DistinguishedName -> DistinguishedName -> [String] -> [ExtensionRaw]
    svExtensionsV3 subDN issDN altNames =
        let
            subjectAltName   = ExtSubjectAltName (AltNameDNS <$> altNames)
        in
            extensionEncode False subjectAltName : usExtensionsV3 KeyUsagePurpose_ServerAuth subDN issDN

    clExtensionsV3 :: DistinguishedName -> DistinguishedName -> [ExtensionRaw]
    clExtensionsV3 =
        usExtensionsV3 KeyUsagePurpose_ClientAuth

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


--
-- INTERNALS / UTILS
--

aesonDropPrefix :: String -> Aeson.Options
aesonDropPrefix pre = Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \s -> maybe s lowerFirst (stripPrefix pre s) }
  where
    lowerFirst :: String -> String
    lowerFirst []    = []
    lowerFirst (h:q) = Char.toLower h : q

forEach :: [a] -> ((Int, a) -> b) -> [b]
forEach xs fn =
    zipWith (curry fn) [0..(length xs - 1)] xs

mkDistinguishedName :: CertConfiguration -> DistinguishedName
mkDistinguishedName CertConfiguration{..} = DistinguishedName
    [ (getObjectID DnOrganization, fromString certOrganization)
    , (getObjectID DnCommonName, fromString certCommonName)
    ]
