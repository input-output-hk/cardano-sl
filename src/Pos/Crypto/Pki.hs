{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}

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

import           Crypto.Hash             as Crypto (SHA256 (..))
import qualified Crypto.PubKey.RSA       as RSA (PrivateKey (..), PublicKey (..),
                                                 generate)
import qualified Crypto.PubKey.RSA.OAEP  as RSA (decryptSafer, defaultOAEPParams, encrypt)
import qualified Crypto.PubKey.RSA.PSS   as RSA (defaultPSSParams, signSafer, verify)
import qualified Crypto.PubKey.RSA.Types as RSA (Error (MessageSizeIncorrect, SignatureTooLong))
import           Data.Binary             (Binary)
import qualified Data.Binary             as Binary
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import qualified Data.Text.Buildable     as Buildable
import           Data.Text.Lazy.Builder  (Builder)
import           Formatting              (Format, bprint, fitLeft, later, (%), (%.))
import           Universum

import qualified Serokell.Util.Base64    as Base64 (decode, encode)

import           Pos.Crypto.Hashing      (hash, hashHexF)
import           Pos.Util                (Raw)

----------------------------------------------------------------------------
-- Some orphan instances
----------------------------------------------------------------------------

deriving instance Ord RSA.PublicKey
deriving instance Ord RSA.PrivateKey

deriving instance Generic RSA.PublicKey
deriving instance Generic RSA.PrivateKey

instance Binary RSA.PublicKey
instance Binary RSA.PrivateKey

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

newtype PublicKey = PublicKey RSA.PublicKey
    deriving (Eq, Ord, Show, Binary)
newtype SecretKey = SecretKey RSA.PrivateKey
    deriving (Eq, Ord, Show, Binary)

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
keyGen = fmap (bimap PublicKey SecretKey) $ liftIO $ RSA.generate 256 0x10001

newtype Encrypted a = Encrypted ByteString
    deriving (Eq, Ord, Show, NFData, Binary)

instance Buildable.Buildable (Encrypted a) where
    build _ = "<encrypted>"

-- | Encode something with 'Binary' and encrypt it.
encrypt
    :: (MonadIO m, Binary a)
    => PublicKey -> a -> m (Encrypted a)
encrypt k = fmap coerce . encryptRaw k . toS . Binary.encode

encryptRaw :: MonadIO m => PublicKey -> ByteString -> m (Encrypted Raw)
encryptRaw (PublicKey k) bs = do
    res <- liftIO $ RSA.encrypt (RSA.defaultOAEPParams Crypto.SHA256) k bs
    case res of
        Right enc -> return (Encrypted enc)
        -- should never happen really
        Left err  -> panic ("encryptRaw: " <> show err)

data DecryptionError
    = MessageSizeIncorrect
    | SignatureTooLong
    | BinaryUnparseable

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
decryptRaw (SecretKey k) (Encrypted x) = do
    res <- liftIO $ RSA.decryptSafer (RSA.defaultOAEPParams Crypto.SHA256) k x
    return $
        case res of
            Left RSA.MessageSizeIncorrect -> Left MessageSizeIncorrect
            Left RSA.SignatureTooLong     -> Left SignatureTooLong
            -- again, should never happen
            Left err                      -> panic ("decrypt: " <> show err)
            Right bs                      -> Right bs

newtype Signature a = Signature ByteString
    deriving (Eq, Ord, Show, NFData, Binary)

instance Buildable.Buildable (Signature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
sign :: (MonadIO m, Binary a) => SecretKey -> a -> m (Signature a)
sign k = fmap coerce . signRaw k . toS . Binary.encode

signRaw :: MonadIO m => SecretKey -> ByteString -> m (Signature Raw)
signRaw (SecretKey k) x = do
    res <- liftIO $ RSA.signSafer (RSA.defaultPSSParams Crypto.SHA256) k x
    case res of
        Right enc -> return (Signature enc)
        Left err  -> panic ("sign: " <> show err)

-- | Verify a signature.
verify :: Binary a => PublicKey -> a -> Signature a -> Bool
verify k x s = verifyRaw k (toS (Binary.encode x)) (coerce s)

verifyRaw :: PublicKey -> ByteString -> Signature Raw -> Bool
verifyRaw (PublicKey k) x (Signature s) =
    RSA.verify (RSA.defaultPSSParams Crypto.SHA256) k x s
