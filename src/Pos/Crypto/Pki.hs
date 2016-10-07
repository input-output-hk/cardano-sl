{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications   #-}

{- | Convenient wrappers over public key crypto (RSA at the moment).

TODO: Do we want to store type info in 'Binary' instance of 'Encrypted' (so
that type mismatch when decoding 'Encrypted' would be caught)?
-}
module Pos.Crypto.Pki
       (
       -- * Keys
         PublicKey
       , SecretKey
       , keyGen
       , toPublic
       , formatFullPublicKey
       , fullPublicKeyF
       , parseFullPublicKey

       -- * Encryption and decryption
       , Encrypted
       , DecryptionError (..)
       , encrypt
       , decrypt

       -- * Signing and verification
       , Signature
       , sign
       , verify

       -- * Versions for raw bytestrings
       , encryptRaw
       , decryptRaw
       , signRaw
       , verifyRaw
       ) where

import           Crypto.Cipher.AES       (AES256)
import           Crypto.Cipher.Types     (cipherInit, ctrCombine, nullIV)
import           Crypto.Error            (CryptoError (..), CryptoFailable (..))
import           Crypto.Hash             (SHA256 (..))
import qualified Crypto.PubKey.RSA       as RSA (PrivateKey (..), PublicKey (..),
                                                 generate)
import qualified Crypto.PubKey.RSA.OAEP  as RSA (decryptSafer, defaultOAEPParams, encrypt)
import qualified Crypto.PubKey.RSA.PSS   as RSA (defaultPSSParams, signSafer, verify)
import qualified Crypto.PubKey.RSA.Types as RSA (Error (MessageSizeIncorrect, SignatureTooLong))
import           Data.Binary             (Binary)
import qualified Data.Binary             as Binary
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import           Data.Hashable           (Hashable)
import           Data.SafeCopy           (SafeCopy (..))
import qualified Data.Text.Buildable     as Buildable
import           Data.Text.Lazy.Builder  (Builder)
import           Formatting              (Format, bprint, fitLeft, later, (%), (%.))
import           Universum

import qualified Serokell.Util.Base64    as Base64 (decode, encode)

import           Pos.Crypto.Hashing      (hash, hashHexF)
import           Pos.Crypto.Random       (runSecureRandom, secureRandomBS)
import           Pos.Util                (Raw, getCopyBinary, putCopyBinary)

----------------------------------------------------------------------------
-- Some orphan instances
----------------------------------------------------------------------------

deriving instance Ord RSA.PublicKey
deriving instance Ord RSA.PrivateKey

deriving instance Generic RSA.PublicKey
deriving instance Generic RSA.PrivateKey

instance Hashable RSA.PublicKey
instance Hashable RSA.PrivateKey

instance Binary RSA.PublicKey
instance Binary RSA.PrivateKey

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

newtype PublicKey = PublicKey RSA.PublicKey
    deriving (Eq, Ord, Show, Binary, Hashable)

newtype SecretKey = SecretKey RSA.PrivateKey
    deriving (Eq, Ord, Show, Binary, Hashable)

instance SafeCopy PublicKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "PublicKey"

instance SafeCopy SecretKey where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "SecretKey"

-- | Generate a public key from a secret key. It's fast, since a secret key
-- actually holds a copy of the public key.
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (RSA.private_pub k)

instance Buildable.Buildable PublicKey where
    -- Hash the key, take first 8 chars (that's how GPG does fingerprinting,
    -- except that their binary representation of the key is different)
    build = bprint ("pub:" % fitLeft 8 %. hashHexF) . hash

instance Buildable.Buildable SecretKey where
    build = bprint ("sec:" % fitLeft 8 %. hashHexF) . hash . toPublic

formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey = Buildable.build . Base64.encode . toS . Binary.encode

fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

parseFullPublicKey :: Text -> Maybe PublicKey
parseFullPublicKey s =
    case Base64.decode s of
        Left _  -> Nothing
        Right b -> case Binary.decodeOrFail (toS b) of
            Left _ -> Nothing
            Right (unconsumed, _, a)
                | BSL.null unconsumed -> Just a
                | otherwise -> Nothing

-- | Generate a key pair.
keyGen :: MonadIO m => m (PublicKey, SecretKey)
keyGen = liftIO . runSecureRandom $
    bimap PublicKey SecretKey <$> RSA.generate 256 0x10001

----------------------------------------------------------------------------
-- AES encryption
----------------------------------------------------------------------------

generateAES256Key :: MonadIO m => m ByteString
generateAES256Key = liftIO $ secureRandomBS (256 `div` 8)

-- | Encrypt data with AES (taken from Crypto.Tutorial).
encryptAES256 :: ByteString -> ByteString -> Either CryptoError ByteString
encryptAES256 key bs = case cipherInit @AES256 key of
    CryptoPassed ctx -> Right (ctrCombine ctx nullIV bs)
    CryptoFailed err -> Left err

-- | Decrypt data with AES.
decryptAES256 :: ByteString -> ByteString -> Either CryptoError ByteString
decryptAES256 = encryptAES256

----------------------------------------------------------------------------
-- Encryption and decryption
----------------------------------------------------------------------------

data Encrypted a = Encrypted
    { encAESKey :: ByteString
    , encData   :: ByteString
    } deriving (Eq, Ord, Show, Generic)

instance NFData (Encrypted a)
instance Binary (Encrypted a)

instance Buildable.Buildable (Encrypted a) where
    build _ = "<encrypted>"

-- | Encode something with 'Binary' and encrypt it.
--
-- TODO: since RSA is very slow, we generate a random AES256 key, encrypt it,
-- and encrypt data with the key. It's what people do (according to Google),
-- but we still violate “never roll your own crypto” badly. If we don't
-- switch to elliptic curve crypto, we should at least switch to HsOpenSSL.
encrypt
    :: (MonadIO m, Binary a)
    => PublicKey -> a -> m (Encrypted a)
encrypt k = fmap coerce . encryptRaw k . toS . Binary.encode

encryptRaw :: MonadIO m => PublicKey -> ByteString -> m (Encrypted Raw)
encryptRaw (PublicKey k) bs = do
    -- Generate random AES256 key
    aesKey <- generateAES256Key
    -- Encrypt it with RSA
    encAESKey <- do
        res <- liftIO $ RSA.encrypt (RSA.defaultOAEPParams SHA256) k aesKey
        case res of
            Right enc -> return enc
            -- should never happen really
            Left err  -> panic ("encryptRaw: " <> show err)
    -- Encrypt the data with the AES key
    let encData = case encryptAES256 aesKey bs of
            Left err -> panic ("encryptRaw: " <> show err)
            Right x  -> x
    return Encrypted{..}

data DecryptionError
    = MessageSizeIncorrect
    | SignatureTooLong
    | BinaryUnparseable
    deriving (Eq, Ord, Show)

-- | Decrypt something and decode it with 'Binary'.
decrypt
    :: (MonadIO m, Binary a)
    => SecretKey -> Encrypted a -> m (Either DecryptionError a)
decrypt k x = do
    res <- liftIO $ decryptRaw k (coerce x)
    return $
        case res of
            Left err -> Left err
            Right bs ->
                case Binary.decodeOrFail (toS bs) of
                    Left _ -> Left BinaryUnparseable
                    Right (unconsumed, _, a)
                        | BSL.null unconsumed -> Right a
                        | otherwise -> Left BinaryUnparseable

decryptRaw
    :: MonadIO m
    => SecretKey
    -> Encrypted Raw
    -> m (Either DecryptionError ByteString)
decryptRaw (SecretKey k) Encrypted{..} = do
    mbAesKey <- liftIO $ RSA.decryptSafer (RSA.defaultOAEPParams SHA256)
                                          k encAESKey
    case mbAesKey of
        Left RSA.MessageSizeIncorrect -> return (Left MessageSizeIncorrect)
        Left RSA.SignatureTooLong     -> return (Left SignatureTooLong)
        -- again, should never happen
        Left err                      -> panic ("decryptRaw: " <> show err)
        Right aesKey -> do
            case decryptAES256 aesKey encData of
                Left err -> panic ("decryptRaw: " <> show err)
                Right d  -> return (Right d)

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

newtype Signature a = Signature ByteString
    deriving (Eq, Ord, Show, NFData, Binary)

instance SafeCopy (Signature a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "Signature"

instance Buildable.Buildable (Signature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
sign :: (MonadIO m, Binary a) => SecretKey -> a -> m (Signature a)
sign k = fmap coerce . signRaw k . toS . Binary.encode

signRaw :: MonadIO m => SecretKey -> ByteString -> m (Signature Raw)
signRaw (SecretKey k) x = do
    res <- liftIO $ RSA.signSafer (RSA.defaultPSSParams SHA256) k x
    case res of
        Right enc -> return (Signature enc)
        Left err  -> panic ("sign: " <> show err)

-- | Verify a signature.
verify :: Binary a => PublicKey -> a -> Signature a -> Bool
verify k x s = verifyRaw k (toS (Binary.encode x)) (coerce s)

verifyRaw :: PublicKey -> ByteString -> Signature Raw -> Bool
verifyRaw (PublicKey k) x (Signature s) =
    RSA.verify (RSA.defaultPSSParams SHA256) k x s
