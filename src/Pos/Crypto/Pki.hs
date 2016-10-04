{- | Convenient wrappers over public key crypto (RSA at the moment).

TODO:

1. We use SHA256 in this module. According to
   <http://security.stackexchange.com/a/112032>, we could've used SHA1 and
   save a bit on message size. I'm not a crypto expert so I'm leaving SHA256.

2. We use blinders everywhere. Again, no idea whether they're needed in our
   case or not.

3. We use 2048-bit keys. Is it too much? Too little? Do we need “security
   past 2030” (i.e. what Wikipedia says about longer keys)?

4. Do we want to store type info in 'Binary' instance of 'Encrypted' (so that
   type mismatch when decoding 'Encrypted' would be caught)?
-}
module Pos.Crypto.Pki
       (
       -- * Keys
         PublicKey
       , SecretKey
       , keyGen

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
import qualified Crypto.PubKey.RSA       as RSA (PrivateKey, PublicKey, generate)
import qualified Crypto.PubKey.RSA.OAEP  as RSA (decryptSafer, defaultOAEPParams, encrypt)
import qualified Crypto.PubKey.RSA.PSS   as RSA (defaultPSSParams, signSafer, verify)
import qualified Crypto.PubKey.RSA.Types as RSA (Error (MessageSizeIncorrect, SignatureTooLong))
import           Data.Binary             (Binary)
import qualified Data.Binary             as Binary
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import qualified Data.Text.Buildable     as Buildable
import           Universum

import           Pos.Util                (Raw)

newtype PublicKey = PublicKey RSA.PublicKey
newtype SecretKey = SecretKey RSA.PrivateKey

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
