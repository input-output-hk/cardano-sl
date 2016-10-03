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
       , PrivateKey
       , keyGen

       -- * Encryption and decryption
       , Encrypted
       , DecryptionError (..)
       , encryptConvert
       , encrypt
       , decryptConvert
       , decrypt

       -- * Signing and verification
       , Signature
       , signConvert
       , sign
       , verifyConvert
       , verify
       ) where

import           Crypto.Hash             as Crypto (SHA256 (..))
import           Crypto.PubKey.RSA       (PrivateKey, PublicKey)
import qualified Crypto.PubKey.RSA       as RSA (generate)
import qualified Crypto.PubKey.RSA.OAEP  as RSA (decryptSafer, defaultOAEPParams, encrypt)
import qualified Crypto.PubKey.RSA.PSS   as RSA (defaultPSSParams, signSafer, verify)
import qualified Crypto.PubKey.RSA.Types as RSA (Error (MessageSizeIncorrect, SignatureTooLong))
import           Data.Binary             (Binary)
import qualified Data.Binary             as Binary
import qualified Data.ByteString.Lazy    as BSL
import           Data.Coerce             (coerce)
import qualified Data.Text.Buildable     as Buildable
import           Universum

-- | Generate a key pair.
keyGen :: MonadIO m => m (PublicKey, PrivateKey)
keyGen = liftIO $ RSA.generate 256 0x10001

newtype Encrypted a = Encrypted ByteString
    deriving (Eq, Ord, Show, NFData, Binary)

instance Buildable.Buildable (Encrypted a) where
    build _ = "<encrypted>"

-- | Encode something with 'Binary' and encrypt it.
encryptConvert
    :: (MonadIO m, Binary a)
    => PublicKey -> a -> m (Encrypted a)
encryptConvert k = fmap coerce . encrypt k . toS . Binary.encode

-- | Encrypt a bytestring.
encrypt :: MonadIO m => PublicKey -> ByteString -> m (Encrypted ByteString)
encrypt k bs = do
    res <- liftIO $ RSA.encrypt (RSA.defaultOAEPParams Crypto.SHA256) k bs
    case res of
        Right enc -> return (Encrypted enc)
        -- should never happen really
        Left err  -> panic ("encrypt: " <> show err)

data DecryptionError
    = MessageSizeIncorrect
    | SignatureTooLong
    | BinaryUnparseable

-- | Decrypt something and decode it with 'Binary'.
decryptConvert
    :: (MonadIO m, Binary a)
    => PrivateKey -> Encrypted a -> m (Either DecryptionError a)
decryptConvert k x = do
    res <- liftIO $ decrypt k (coerce x)
    return $
        case res of
            Left err -> Left err
            Right bs ->
                case Binary.decodeOrFail (toS bs) of
                    Left _ -> Left BinaryUnparseable
                    Right (unconsumed, _, a)
                        | BSL.null unconsumed -> Right a
                        | otherwise -> Left BinaryUnparseable

-- | Decrypt a bytestring.
decrypt
    :: MonadIO m
    => PrivateKey
    -> Encrypted ByteString
    -> m (Either DecryptionError ByteString)
decrypt k (Encrypted x) = do
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
signConvert
    :: (MonadIO m, Binary a)
    => PrivateKey -> a -> m (Signature a)
signConvert k = fmap coerce . sign k . toS . Binary.encode

-- | Sign a bytestring.
sign :: MonadIO m => PrivateKey -> ByteString -> m (Signature ByteString)
sign k x = do
    res <- liftIO $ RSA.signSafer (RSA.defaultPSSParams Crypto.SHA256) k x
    case res of
        Right enc -> return (Signature enc)
        Left err  -> panic ("sign: " <> show err)

-- | Verify a signature.
verifyConvert :: Binary a => PublicKey -> a -> Signature a -> Bool
verifyConvert k x s = verify k (toS (Binary.encode x)) (coerce s)

verify :: PublicKey -> ByteString -> Signature ByteString -> Bool
verify k x (Signature s) =
    RSA.verify (RSA.defaultPSSParams Crypto.SHA256) k x s
