{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell    #-}

-- | Signing done with public/private keys.
module Pos.Crypto.Signing
       (
       -- * Keys
         PublicKey
       , SecretKey
       , keyGen
       , deterministicKeyGen
       , toPublic
       , formatFullPublicKey
       , fullPublicKeyF
       , parseFullPublicKey

       -- * Signing and verification
       , Signature
       , sign
       , checkSig

       , Signed
       , mkSigned
       , signedValue
       , signedSig

       -- * Versions for raw bytestrings
       , signRaw
       , verifyRaw
       ) where

import qualified Crypto.Sign.Ed25519    as Ed25519
import           Data.Aeson             (ToJSON (toJSON))
import           Data.Binary            (Binary)
import qualified Data.Binary            as Binary
import qualified Data.Binary.Get        as Binary
import qualified Data.Binary.Put        as Binary
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable)
import           Data.MessagePack       (MessagePack)
import qualified Data.MessagePack       as MP (fromObject, toObject)
import           Data.SafeCopy          (SafeCopy (..), base, deriveSafeCopySimple)
import qualified Data.Serialize         as Cereal
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, int, later, sformat, stext, (%))
import           Universum

import qualified Serokell.Util.Base64   as Base64 (decode, encode)

import           Pos.Crypto.Hashing     (hash, shortHashF)
import           Pos.Crypto.Random      (secureRandomBS)
import           Pos.Util               (Raw, getCopyBinary, putCopyBinary)

----------------------------------------------------------------------------
-- Some orphan instances
----------------------------------------------------------------------------

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

secretKeyLength, publicKeyLength, signatureLength :: Int
secretKeyLength = 64
publicKeyLength = 32
signatureLength = 64

putAssertLength :: Monad m => Text -> Int -> ByteString -> m ()
putAssertLength typeName expectedLength bs =
    when (BS.length bs /= expectedLength) $ panic $
        sformat ("put@"%stext%": expected length "%int%", not "%int)
                typeName expectedLength (BS.length bs)

-- Binary

instance Binary Ed25519.PublicKey where
    put (Ed25519.PublicKey k) = do
        putAssertLength "PublicKey" publicKeyLength k
        Binary.putByteString k
    get = Ed25519.PublicKey <$> Binary.getByteString publicKeyLength

instance Binary Ed25519.SecretKey where
    put (Ed25519.SecretKey k) = do
        putAssertLength "SecretKey" secretKeyLength k
        Binary.putByteString k
    get = Ed25519.SecretKey <$> Binary.getByteString secretKeyLength

instance Binary Ed25519.Signature where
    put (Ed25519.Signature s) = do
        putAssertLength "Signature" signatureLength s
        Binary.putByteString s
    get = Ed25519.Signature <$> Binary.getByteString signatureLength

-- Cereal

instance Cereal.Serialize Ed25519.PublicKey where
    put (Ed25519.PublicKey k) = do
        putAssertLength "PublicKey" publicKeyLength k
        Cereal.putByteString k
    get = Ed25519.PublicKey <$> Cereal.getByteString publicKeyLength

instance Cereal.Serialize Ed25519.SecretKey where
    put (Ed25519.SecretKey k) = do
        putAssertLength "SecretKey" secretKeyLength k
        Cereal.putByteString k
    get = Ed25519.SecretKey <$> Cereal.getByteString secretKeyLength

instance Cereal.Serialize Ed25519.Signature where
    put (Ed25519.Signature s) = do
        putAssertLength "Signature" signatureLength s
        Cereal.putByteString s
    get = Ed25519.Signature <$> Cereal.getByteString signatureLength

-- MessagePack

instance MessagePack Ed25519.PublicKey where
    toObject (Ed25519.PublicKey k) = MP.toObject k
    fromObject = fmap Ed25519.PublicKey . MP.fromObject

instance MessagePack Ed25519.SecretKey where
    toObject (Ed25519.SecretKey k) = MP.toObject k
    fromObject = fmap Ed25519.SecretKey . MP.fromObject

instance MessagePack Ed25519.Signature where
    toObject (Ed25519.Signature s) = MP.toObject s
    fromObject = fmap Ed25519.Signature . MP.fromObject

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.PublicKey'.
newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData,
              Binary, Cereal.Serialize, Hashable)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype SecretKey = SecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData,
              Binary, Cereal.Serialize, Hashable)

instance MessagePack PublicKey
instance MessagePack SecretKey

instance SafeCopy PublicKey where
    -- an empty declaration uses Cereal.Serialize instance by default
instance SafeCopy SecretKey where
    -- an empty declaration uses Cereal.Serialize instance by default

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (Ed25519.toPublicKey k)

instance Buildable.Buildable PublicKey where
    -- Hash the key, take first 8 chars (that's how GPG does fingerprinting,
    -- except that their binary representation of the key is different)
    build = bprint ("pub:"%shortHashF) . hash

instance Buildable.Buildable SecretKey where
    build = bprint ("sec:"%shortHashF) . hash . toPublic

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey = Buildable.build . Base64.encode . BSL.toStrict . Binary.encode

-- | Specialized formatter for 'PublicKey' to show it in base64.
fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

-- | Parse 'PublicKey' from base64 encoded string.
parseFullPublicKey :: Text -> Maybe PublicKey
parseFullPublicKey s =
    case Base64.decode s of
        Left _  -> Nothing
        Right b -> case Binary.decodeOrFail (BSL.fromStrict b) of
            Left _ -> Nothing
            Right (unconsumed, _, a)
                | BSL.null unconsumed -> Just a
                | otherwise -> Nothing

-- | Generate a key pair.
keyGen :: MonadIO m => m (PublicKey, SecretKey)
keyGen = liftIO $ do
    seed <- secureRandomBS 32
    case Ed25519.createKeypairFromSeed_ seed of
        Nothing -> panic "Pos.Crypto.Signing.keyGen:\
                         \ createKeypairFromSeed_ failed"
        Just (pk, sk) -> return (PublicKey pk, SecretKey sk)

-- | Create key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> Maybe (PublicKey, SecretKey)
deterministicKeyGen seed =
    bimap PublicKey SecretKey <$> Ed25519.createKeypairFromSeed_ seed

instance ToJSON PublicKey where
    toJSON = toJSON . sformat fullPublicKeyF

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.Signature'.
newtype Signature a = Signature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Binary, Hashable)

instance MessagePack (Signature a)

instance SafeCopy (Signature a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "Signature"

instance Buildable.Buildable (Signature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
sign :: (Binary a) => SecretKey -> a -> Signature a
sign k = coerce . signRaw k . BSL.toStrict . Binary.encode

-- | Alias for constructor.
signRaw :: SecretKey -> ByteString -> Signature Raw
signRaw (SecretKey k) x = Signature (Ed25519.dsign k x)

-- | Verify a signature.
checkSig :: Binary a => PublicKey -> a -> Signature a -> Bool
checkSig k x s = verifyRaw k (BSL.toStrict (Binary.encode x)) (coerce s)

-- | Verify raw 'ByteString'.
verifyRaw :: PublicKey -> ByteString -> Signature Raw -> Bool
verifyRaw (PublicKey k) x (Signature s) = Ed25519.dverify k x s

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a              -- ^ Value to be signed
    , signedSig   :: !(Signature a)  -- ^ 'Signature' of 'signedValue'
    } deriving (Show, Eq, Ord, Generic)

instance Binary a => Binary (Signed a)
instance MessagePack a => MessagePack (Signed a)

-- | Smart constructor for 'Signed' data type with proper signing.
mkSigned :: (Binary a) => SecretKey -> a -> Signed a
mkSigned sk x = Signed x (sign sk x)

deriveSafeCopySimple 0 'base ''Signed
