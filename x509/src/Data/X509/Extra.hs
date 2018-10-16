{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

--
-- | Cryptographic & Data.X509 specialized methods for RSA with SHA256
--
module Data.X509.Extra
    (
    -- * RSA/SHA-256 Applied Constructors
      signAlgRSA256
    , signCertificate
    , genRSA256KeyPair
    , validateDefaultWithIP
    , validateCertificate

    -- * Utils
    , failIfReasons
    , parseSAN
    , isServerCertificate
    , isClientCertificate

    -- * RSA Encode PEM
    , EncodePEM (..)

    -- * Effectful IO Functions
    , writeCredentials
    , writeCertificate

    -- * Re-Export
    , module Data.X509
    , module Data.X509.Validation
    ) where

import           Universum

import           Crypto.Hash.Algorithms (SHA256 (..))
import           Crypto.PubKey.RSA (PrivateKey (..), PublicKey (..), generate)
import           Crypto.PubKey.RSA.PKCS15 (signSafer)
import           Crypto.Random.Types (MonadRandom)
import           Data.ASN1.BinaryEncoding (DER (..))
import           Data.ASN1.Encoding (encodeASN1)
import           Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..),
                     asn1CharacterToString)
import           Data.ByteString (ByteString)
import           Data.Default.Class
import           Data.List (intercalate)
import           Data.X509
import           Data.X509.CertificateStore (CertificateStore,
                     makeCertificateStore)
import           Data.X509.Validation
import           Net.IPv4 (IPv4 (..))
import           Net.IPv6 (IPv6 (..))

import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import qualified Net.IP as IP


--
-- RSA/SHA-256 Applied Constructors
--

-- | Algorithm Signature for RSA with SHA256
signAlgRSA256 :: SignatureALG
signAlgRSA256 =
    SignatureALG HashSHA256 PubKeyALG_RSA


-- | Sign a X.509 certificate using RSA-PKCS1.5 with SHA256
signCertificate :: (MonadFail m, MonadRandom m) => PrivateKey -> Certificate -> m SignedCertificate
signCertificate key =
    objectToSignedExactF signSHA256
  where
    signSHA256 :: (MonadFail m, MonadRandom m) => ByteString -> m (ByteString, SignatureALG)
    signSHA256 =
        signSafer (Just SHA256) key >=> orFail

    orFail :: MonadFail m => Either RSA.Error ByteString -> m (ByteString, SignatureALG)
    orFail =
        either (fail . show) (return . (,signAlgRSA256))


-- | Drop-in replacement for 'validateDefault' but with support for IP SAN
validateDefaultWithIP
    :: CertificateStore
    -> ValidationCache
    -> ServiceID
    -> CertificateChain
    -> IO [FailedReason]
validateDefaultWithIP =
    validate HashSHA256 hooks checks
  where
    hooks  = defaultHooks { hookValidateName = validateCertificateName }
    checks = defaultChecks


-- | Validate a X.509 certificate using SHA256 hash and a given CA. This is
-- merely to verify that we aren't generating invalid certificates.
validateCertificate
    :: SignedCertificate
    -> ValidationChecks
    -> ServiceID
    -> SignedCertificate
    -> IO [FailedReason]
validateCertificate caCert checks sid cert =
    validate HashSHA256 hooks checks store def sid chain
  where
    hooks = defaultHooks { hookValidateName = validateCertificateName }
    store = makeCertificateStore [caCert]
    chain = CertificateChain [cert]


-- | Tell whether a Certificate is a Server certificate
isServerCertificate
    :: SignedCertificate
    -> Bool
isServerCertificate =
    elem KeyUsagePurpose_ServerAuth . getKeyUsagePurposes


-- | Tell whether a Certificate is  Client certificate
isClientCertificate
    :: SignedCertificate
    -> Bool
isClientCertificate =
    elem KeyUsagePurpose_ClientAuth . getKeyUsagePurposes


-- | Generate a new RSA-256 key pair
genRSA256KeyPair :: IO (PublicKey, PrivateKey)
genRSA256KeyPair =
    generate 256 65537


--
-- EncodePEM
--

-- | Encode an artifact to PEM (i.e. base64 DER with header & footer)
class EncodePEM a where
    encodePEM :: a -> ByteString
    encodePEMRaw :: (ByteString, a -> ByteString, ByteString) -> a -> ByteString
    encodePEMRaw (header, encodeDER, footer) a =
        BS.concat
            [ header
            , "\n"
            , BS.intercalate "\n" (mkGroupsOf 64 $ Base64.encode $ encodeDER a)
            , "\n"
            , footer
            ]
      where
        mkGroupsOf :: Int -> ByteString -> [ByteString]
        mkGroupsOf n xs
            | BS.length xs == 0 = []
            | otherwise         = (BS.take n xs) : mkGroupsOf n (BS.drop n xs)

instance EncodePEM PrivateKey where
    encodePEM = encodePEMRaw
        ( "-----BEGIN RSA PRIVATE KEY-----"
        , encodeDERRSAPrivateKey
        , "-----END RSA PRIVATE KEY-----"
        )

instance EncodePEM (SignedExact Certificate) where
    encodePEM = encodePEMRaw
        ( "-----BEGIN CERTIFICATE-----"
        , encodeSignedObject
        , "-----END CERTIFICATE-----"
        )

--
-- Utils
--

-- | Fail with the given reason if any, does nothing otherwise
failIfReasons
    :: MonadFail m
    => [FailedReason]
    -> m ()
failIfReasons = \case
    [] -> return ()
    xs -> fail $ "Generated invalid certificate: " ++ intercalate ", " (map show xs)


-- | Parse a Subject Alternative Name (SAN) from a raw string
parseSAN :: String -> AltName
parseSAN name =
    case IP.decode (toText name) of
        Just ip ->
            AltNameIP $ IP.case_ ipv4ToBS ipv6ToBS ip

        Nothing ->
            AltNameDNS name
  where
    -- NOTE
    -- Here, we define custom encoding functions and aren't using the ones
    -- defined in `Net.IP`, `Net.IPv4` or `Net.IPv6`.
    -- Those methods lead to invalid encodings for the underlying x509 certificates.
    --
    -- From the RFC 3779 (https://datatracker.ietf.org/doc/rfc3779):
    --
    -- > IP v4 address - a 32-bit identifier written as four decimal numbers,
    -- >    each in the range 0 to 255, separated by a ".".  10.5.0.5 is an
    -- >    example of an IPv4 address.
    -- >
    -- > IP v6 address - a 128-bit identifier written as eight hexadecimal
    -- >    quantities, each in the range 0 to ffff, separated by a ":".
    -- >    2001:0:200:3:0:0:0:1 is an example of an IPv6 address.  One string
    -- >    of :0: fields may be replaced by "::", thus 2001:0:200:3::1
    -- >    represents the same address as the immediately preceding example.
    ipv4ToBS :: IPv4 -> ByteString
    ipv4ToBS (IPv4 bytes) =
        BL.toStrict $ BS.toLazyByteString (BS.word32BE bytes)

    ipv6ToBS :: IPv6 -> ByteString
    ipv6ToBS (IPv6 a b) =
        BL.toStrict $ BS.toLazyByteString (BS.word64BE a <> BS.word64BE b)


--
-- Effectful IO Functions
--

-- | Write a certificate and its private key to the given location
writeCredentials
    :: FilePath
    -> (PrivateKey, SignedCertificate)
    -> IO ()
writeCredentials filename (key, cert) = do
    BS.writeFile (filename <> ".pem") (BS.concat [keyBytes, "\n", certBytes])
    BS.writeFile (filename <> ".key") keyBytes
    BS.writeFile (filename <> ".crt") certBytes
  where
    keyBytes  = encodePEM key
    certBytes = encodePEM cert


-- | Write a certificate to the given location
writeCertificate
    :: FilePath
    -> SignedCertificate
    -> IO ()
writeCertificate filename cert =
    BS.writeFile (filename <> ".crt") (encodePEM cert)


--
-- Internals
--

-- | Encode a RSA private key as DER (Distinguished Encoding Rule) binary format
encodeDERRSAPrivateKey :: PrivateKey -> ByteString
encodeDERRSAPrivateKey =
    BL.toStrict . encodeASN1 DER . rsaToASN1
  where
    -- | RSA Private Key Syntax, see https://tools.ietf.org/html/rfc3447#appendix-A.1
    rsaToASN1 :: PrivateKey -> [ASN1]
    rsaToASN1 (PrivateKey (PublicKey _ n e) d p q dP dQ qInv) =
        [ Start Sequence
        , IntVal 0
        , IntVal n
        , IntVal e
        , IntVal d
        , IntVal p
        , IntVal q
        , IntVal dP
        , IntVal dQ
        , IntVal qInv
        , End Sequence
        ]


-- | Extract KeyUsagePurpose from a certificate, if any
getKeyUsagePurposes
    :: SignedCertificate
    -> [ExtKeyUsagePurpose]
getKeyUsagePurposes =
    fromMaybe []
    . fmap (\(ExtExtendedKeyUsage ps) -> ps)
    . extensionGet
    . certExtensions
    . getCertificate


-- | Hook to validate a certificate name. It only validates DNS and IPs names
-- against the provided hostname. It fails otherwise.
validateCertificateName :: HostName -> Certificate -> [FailedReason]
validateCertificateName fqhn =
    case parseSAN fqhn of
        AltNameIP bytes ->
            validateCertificateIP bytes
        _ ->
            validateCertificateDNS fqhn


-- | Hook to validate certificate DNS, using the default hook from
-- x509-validation which does exactly that.
validateCertificateDNS :: HostName -> Certificate -> [FailedReason]
validateCertificateDNS =
    hookValidateName defaultHooks


-- | Basic validation against the host if it turns out to be an IP address
validateCertificateIP :: ByteString -> Certificate -> [FailedReason]
validateCertificateIP ip cert =
    let
        commonName :: Maybe ByteString
        commonName =
            toCommonName =<< getDnElement DnCommonName (certSubjectDN cert)

        altNames :: [ByteString]
        altNames =
            maybe [] toAltName $ extensionGet $ certExtensions cert

        toAltName :: ExtSubjectAltName -> [ByteString]
        toAltName (ExtSubjectAltName sans) =
            catMaybes $ flip map sans $ \case
                AltNameIP bytes -> Just bytes
                _               -> Nothing

        toCommonName :: ASN1CharacterString -> Maybe ByteString
        toCommonName =
            fmap B8.pack . asn1CharacterToString
    in
        if ip `elem` (maybeToList commonName ++ altNames) then
            []
        else
            [NameMismatch $ B8.unpack ip]
