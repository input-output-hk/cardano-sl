--
-- | Cryptographic & Data.X509 specialized methods for RSA with SHA256
--
module Data.X509.Extra
    ( signAlgRSA256
    , signCertificate
    , genRSA256KeyPair
    , encodeRSAPrivateKey
    ) where

import           Control.Monad.Trans.Except (ExceptT (..), withExceptT)
import           Crypto.Hash.Algorithms (SHA256 (..))
import           Crypto.PubKey.RSA (PrivateKey (..), PublicKey (..), generate)
import           Crypto.PubKey.RSA.PKCS15 (signSafer)
import           Data.ASN1.BinaryEncoding (DER (..))
import           Data.ASN1.Encoding (encodeASN1)
import           Data.ASN1.Types (ASN1 (..), ASN1ConstructionType (..))
import           Data.ByteString (ByteString)
import           Data.X509

import qualified Data.ByteString.Lazy as BL


-- | Algorithm Signature for RSA with SHA256
signAlgRSA256 :: SignatureALG
signAlgRSA256 =
    SignatureALG HashSHA256 PubKeyALG_RSA

-- | Sign a X.509 certificate using RSA-PKCS1.5 with SHA256
signCertificate :: PrivateKey -> Certificate -> ExceptT String IO SignedCertificate
signCertificate key =
    objectToSignedExactF signSHA256
  where
    signSHA256 :: ByteString -> ExceptT String IO (ByteString, SignatureALG)
    signSHA256 =
        fmap (,signAlgRSA256) . withExceptT show . ExceptT . signSafer (Just SHA256) key

-- | Generate a new RSA-256 key pair
genRSA256KeyPair :: IO (PublicKey, PrivateKey)
genRSA256KeyPair =
    generate 256 65537

-- | Encode a RSA private key as DER (Distinguished Encoding Rule) binary format
encodeRSAPrivateKey :: PrivateKey -> ByteString
encodeRSAPrivateKey =
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
