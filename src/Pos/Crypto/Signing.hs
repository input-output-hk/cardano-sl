{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Signing done with public/private keys.
module Pos.Crypto.Signing
       (
       -- * Keys
         PublicKey (..)
       , SecretKey (..)
       , keyGen
       , deterministicKeyGen
       , toPublic
       , formatFullPublicKey
       , fullPublicKeyF
       , parseFullPublicKey

       -- * Signing and verification
       , Signature (..)
       , sign
       , checkSig

       , Signed (..)
       , mkSigned

       -- * Versions for raw bytestrings
       , signRaw
       , verifyRaw

       -- remove it when getting rid of cereal!
       , secretKeyLength
       , publicKeyLength
       , signatureLength
       , putAssertLength

       -- * Proxy signature scheme
       , ProxyISignature (..)
       , proxyISign
       , proxyICheckSig

       , ProxyCert (..)
       , createProxyCert
       , ProxySecretKey (..)
       , createProxySecretKey
       , ProxyDSignature (..)
       , proxyDSign
       , proxyDVerify
       ) where

import           Control.Monad.Fail     (fail)
import qualified Crypto.Sign.Ed25519    as Ed25519
import           Data.Aeson             (ToJSON (toJSON))
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (SafeCopy (..), contain, safeGet, safePut)
import qualified Data.Serialize         as Cereal
import qualified Data.Text.Buildable    as Buildable
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, int, later, sformat, stext, (%))
import           Universum

import qualified Serokell.Util.Base64   as Base64 (decode, encode)

import           Pos.Binary.Class       (Bi)
import qualified Pos.Binary.Class       as Bi
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

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.PublicKey'.
newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData,
              Cereal.Serialize, Hashable)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype SecretKey = SecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData,
              Cereal.Serialize, Hashable)


-- TODO GET RID OF CEREAL STUFF HERE

secretKeyLength, publicKeyLength, signatureLength :: Int
secretKeyLength = 64
publicKeyLength = 32
signatureLength = 64

putAssertLength :: Monad m => Text -> Int -> ByteString -> m ()
putAssertLength typeName expectedLength bs =
    when (BS.length bs /= expectedLength) $ panic $
        sformat ("put@"%stext%": expected length "%int%", not "%int)
                typeName expectedLength (BS.length bs)

-- Cereal (for safecopy -- todo write safecopy instead)

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

instance SafeCopy PublicKey where
    -- an empty declaration uses Cereal.Serialize instance by default
instance SafeCopy SecretKey where
    -- an empty declaration uses Cereal.Serialize instance by default

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (Ed25519.toPublicKey k)

instance Bi PublicKey => Buildable.Buildable PublicKey where
    -- Hash the key, take first 8 chars (that's how GPG does fingerprinting,
    -- except that their binary representation of the key is different)
    build = bprint ("pub:"%shortHashF) . hash

instance Bi PublicKey => Buildable.Buildable SecretKey where
    build = bprint ("sec:"%shortHashF) . hash . toPublic

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey (PublicKey pk) =
    Buildable.build . Base64.encode . Ed25519.unPublicKey $ pk

-- | Specialized formatter for 'PublicKey' to show it in base64.
fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

-- | Parse 'PublicKey' from base64 encoded string.
parseFullPublicKey :: (Bi PublicKey) => Text -> Maybe PublicKey
parseFullPublicKey s =
    case Base64.decode s of
        Left _  -> Nothing
        Right b -> case Bi.decodeOrFail (BSL.fromStrict b) of
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
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Bi (Signature a) => SafeCopy (Signature a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "Signature"

instance Buildable.Buildable (Signature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
sign :: Bi a => SecretKey -> a -> Signature a
sign k = coerce . signRaw k . BSL.toStrict . Bi.encode

-- | Alias for constructor.
signRaw :: SecretKey -> ByteString -> Signature Raw
signRaw (SecretKey k) x = Signature (Ed25519.dsign k x)

-- | Verify a signature.
checkSig :: Bi a => PublicKey -> a -> Signature a -> Bool
checkSig k x s = verifyRaw k (BSL.toStrict (Bi.encode x)) (coerce s)

-- | Verify raw 'ByteString'.
verifyRaw :: PublicKey -> ByteString -> Signature Raw -> Bool
verifyRaw (PublicKey k) x (Signature s) = Ed25519.dverify k x s

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a              -- ^ Value to be signed
    , signedSig   :: !(Signature a)  -- ^ 'Signature' of 'signedValue'
    } deriving (Show, Eq, Ord, Generic)

-- | Smart constructor for 'Signed' data type with proper signing.
mkSigned :: (Bi a) => SecretKey -> a -> Signed a
mkSigned sk x = Signed x (sign sk x)

instance (Bi (Signature a), Bi a) => SafeCopy (Signed a) where
    putCopy (Signed v s) = contain $ safePut (Bi.encode (v,s))
    getCopy = contain $ do
        bs <- safeGet
        case Bi.decodeFull bs of
            Left err    -> fail $ "getCopy@SafeCopy: " ++ err
            Right (v,s) -> pure $ Signed v s

----------------------------------------------------------------------------
-- Proxy signing
----------------------------------------------------------------------------

-- | Signature produced by issuer
newtype ProxyISignature a = ProxyISignature { unProxyISignature :: Ed25519.Signature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance Buildable.Buildable (ProxyISignature a) where
    build _ = "<proxy_i_signature>"

-- | Raw bytestring signing
proxyISignRaw :: SecretKey -> ByteString -> ProxyISignature Raw
proxyISignRaw (SecretKey k) m =
    ProxyISignature (Ed25519.dsign k $ "11" `BS.append` m)

-- | Getting signature from binary representation of value
proxyISign :: Bi a => SecretKey -> a -> ProxyISignature a
proxyISign k = coerce . proxyISignRaw k . BSL.toStrict . Bi.encode

-- | Raw bytestring verification
proxyIVerifyRaw :: PublicKey -> ByteString -> ProxyISignature Raw -> Bool
proxyIVerifyRaw (PublicKey k) m (ProxyISignature s) =
    Ed25519.dverify k ("11" `BS.append` m) s

-- | Verify a proxy issuer signature using value's binary
-- representation
proxyICheckSig :: Bi a => PublicKey -> a -> ProxyISignature a -> Bool
proxyICheckSig k m s = proxyIVerifyRaw k (BSL.toStrict (Bi.encode m)) (coerce s)

-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert { unProxyCert :: Ed25519.Signature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
createProxyCert :: (Bi w) => SecretKey -> PublicKey -> w -> ProxyCert w
createProxyCert (SecretKey issuerSk) (PublicKey delegatePk) o =
    coerce $
    ProxyCert $
    Ed25519.dsign issuerSk $
    mconcat
        ["00", Ed25519.unPublicKey delegatePk, BSL.toStrict $ Bi.encode o]

-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data ProxySecretKey w = ProxySecretKey w (ProxyCert w)
    deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySecretKey w)
instance Hashable w => Hashable (ProxySecretKey w)

-- | Creates proxy secret key
createProxySecretKey :: (Bi w) => SecretKey -> PublicKey -> w -> ProxySecretKey w
createProxySecretKey issuerSk delegatePk w =
    ProxySecretKey w $ createProxyCert issuerSk delegatePk w

-- | Delegate signature made with certificate-based permission. @a@
-- stays for message type used in proxy (ω in the implementation
-- notes), @b@ for type of message signed.
data ProxyDSignature w a = ProxyDSignature
    { pdOmega      :: w
    , pdDelegatePk :: PublicKey
    , pdCert       :: ProxyCert w
    , pdSig        :: Ed25519.Signature
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxyDSignature w a)
instance Hashable w => Hashable (ProxyDSignature w a)

-- | Make a proxy delegate signature with help of certificate.
proxyDSign
    :: (Bi a)
    => SecretKey -> PublicKey -> ProxySecretKey w -> a -> ProxyDSignature w a
proxyDSign sk@(SecretKey delegateSk) (PublicKey issuerPk) (ProxySecretKey o cert) m =
    ProxyDSignature
    { pdOmega = o
    , pdDelegatePk = toPublic sk
    , pdCert = cert
    , pdSig = sigma
    }
  where
    sigma =
        Ed25519.dsign delegateSk $
        mconcat ["01", Ed25519.unPublicKey issuerPk, BSL.toStrict $ Bi.encode m]

-- | Verify delegated signature given issuer's pk, signature, message
-- space predicate and message itself.
proxyDVerify
    :: (Bi w, Bi a)
    => PublicKey -> ProxyDSignature w a -> (w -> Bool) -> a -> Bool
proxyDVerify (PublicKey issuerPk) ProxyDSignature {..} omegaPred m =
    and [predCorrect, certValid, sigValid]
  where
    PublicKey pdDelegatePkRaw = pdDelegatePk
    predCorrect = omegaPred pdOmega
    certValid =
        Ed25519.dverify
            issuerPk
            (mconcat
                 [ "00"
                 , Ed25519.unPublicKey pdDelegatePkRaw
                 , BSL.toStrict $ Bi.encode pdOmega
                 ])
            (unProxyCert pdCert)
    sigValid =
        Ed25519.dverify
            pdDelegatePkRaw
            (mconcat
                 [ "01"
                 , Ed25519.unPublicKey issuerPk
                 , BSL.toStrict $ Bi.encode m
                 ])
            pdSig
