{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase        #-}

--
-- | Cryptographic & Data.X509 specialized methods for RSA with SHA256
--
module Data.X509.Extra
    ( signAlgRSA256
    , signCertificate
    , validateSHA256
    , failIfReasons
    , genRSA256KeyPair
    , encodeDERRSAPrivateKey
    , writeCredentials
    , writeCertificate
    , EncodePEM (..)
    ) where

import           Universum

import           Crypto.Hash.Algorithms (SHA256 (..))
import           Crypto.PubKey.RSA (PrivateKey (..), PublicKey (..), generate)
import           Crypto.PubKey.RSA.PKCS15 (signSafer)
import           Crypto.Random.Types (MonadRandom)
import           Data.ASN1.BinaryEncoding (DER (..))
import           Data.ASN1.Encoding (encodeASN1)
import           Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..))
import           Data.ByteString (ByteString)
import           Data.Default.Class
import           Data.List (intercalate)
import           Data.X509
import           Data.X509.CertificateStore (makeCertificateStore)
import           Data.X509.Validation (FailedReason, ServiceID, ValidationChecks (..), defaultHooks,
                                       validate)

import qualified Crypto.PubKey.RSA.Types as RSA
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Lazy as BL

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


-- | Validate a X.509 certificate using SHA256 hash and a given CA. This is
-- merely to verify that we aren't generating invalid certificates.
validateSHA256
    :: SignedCertificate
    -> ValidationChecks
    -> ServiceID
    -> SignedCertificate
    -> IO [FailedReason]
validateSHA256 caCert checks sid cert =
    validate HashSHA256 defaultHooks checks store def sid chain
  where
    store = makeCertificateStore [caCert]
    chain = CertificateChain [cert]


-- | Fail with the given reason if any, does nothing otherwise
failIfReasons
    :: MonadFail m
    => [FailedReason]
    -> m ()
failIfReasons = \case
    [] -> return ()
    xs -> fail $ "Generated invalid certificate: " ++ intercalate ", " (map show xs)


-- | Generate a new RSA-256 key pair
genRSA256KeyPair :: IO (PublicKey, PrivateKey)
genRSA256KeyPair =
    generate 256 65537


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
