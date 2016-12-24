{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
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

       -- * Proxy signature scheme
       , ProxyCert (..)
       , createProxyCert
       , ProxySecretKey (..)
       , createProxySecretKey
       , ProxySignature (..)
       , proxySign
       , proxyVerify
       , checkProxySecretKey
       ) where

import           Control.Monad.Fail     (fail)
import qualified Crypto.Sign.Ed25519    as Ed25519
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable)
import           Data.SafeCopy          (SafeCopy (..), base, contain,
                                         deriveSafeCopySimple, safeGet, safePut)
import qualified Data.Text.Buildable    as B
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, later, (%))
import qualified Serokell.Util.Base64   as Base64 (decode, encode)
import           Universum

import           Pos.Binary.Class       (Bi)
import qualified Pos.Binary.Class       as Bi
import           Pos.Crypto.Hashing     (hash, shortHashF)
import           Pos.Crypto.Random      (secureRandomBS)
import           Pos.Util               (Raw)

----------------------------------------------------------------------------
-- Some orphan instances
----------------------------------------------------------------------------

instance Hashable Ed25519.PublicKey
instance Hashable Ed25519.SecretKey
instance Hashable Ed25519.Signature

instance NFData Ed25519.PublicKey
instance NFData Ed25519.SecretKey
instance NFData Ed25519.Signature

deriveSafeCopySimple 0 'base ''Ed25519.PublicKey
deriveSafeCopySimple 0 'base ''Ed25519.SecretKey
deriveSafeCopySimple 0 'base ''Ed25519.Signature

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.PublicKey'.
newtype PublicKey = PublicKey Ed25519.PublicKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

-- | Wrapper around 'Ed25519.SecretKey'.
newtype SecretKey = SecretKey Ed25519.SecretKey
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

deriveSafeCopySimple 0 'base ''PublicKey
deriveSafeCopySimple 0 'base ''SecretKey

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (Ed25519.secretToPublicKey k)

instance Bi PublicKey => B.Buildable PublicKey where
    -- Hash the key, take first 8 chars (that's how GPG does fingerprinting,
    -- except that their binary representation of the key is different)
    build = bprint ("pub:"%shortHashF) . hash

instance Bi PublicKey => B.Buildable SecretKey where
    build = bprint ("sec:"%shortHashF) . hash . toPublic

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey (PublicKey pk) =
    B.build . Base64.encode . Ed25519.openPublicKey $ pk

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

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Wrapper around 'Ed25519.Signature'.
newtype Signature a = Signature Ed25519.Signature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance SafeCopy (Signature a) where
    putCopy (Signature sig) = contain $ safePut sig
    getCopy = contain $ Signature <$> safeGet

instance B.Buildable (Signature a) where
    build _ = "<signature>"

-- | Encode something with 'Binary' and sign it.
sign :: Bi a => SecretKey -> a -> Signature a
sign k = coerce . signRaw k . BSL.toStrict . Bi.encode

-- | Alias for constructor.
signRaw :: SecretKey -> ByteString -> Signature Raw
signRaw (SecretKey k) x = Signature (Ed25519.dsign k x)

-- CHECK: @checkSig
-- | Verify a signature.
-- #verifyRaw
checkSig :: Bi a => PublicKey -> a -> Signature a -> Bool
checkSig k x s = verifyRaw k (BSL.toStrict (Bi.encode x)) (coerce s)

-- CHECK: @verifyRaw
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

-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert { unProxyCert :: Ed25519.Signature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance B.Buildable (ProxyCert w) where
    build _ = "<proxy_cert>"

-- Written by hand, because @deriveSafeCopySimple@ generates redundant
-- constraint (SafeCopy w) though it's phantom.
instance SafeCopy (ProxyCert w) where
    putCopy (ProxyCert sig) = contain $ safePut sig
    getCopy = contain $ ProxyCert <$> safeGet

-- | Proxy certificate creation from secret key of issuer, public key
-- of delegate and the message space ω.
createProxyCert :: (Bi w) => SecretKey -> PublicKey -> w -> ProxyCert w
createProxyCert (SecretKey issuerSk) (PublicKey delegatePk) o =
    coerce $
    ProxyCert $
    Ed25519.dsign issuerSk $
    mconcat
        ["00", Ed25519.openPublicKey delegatePk, BSL.toStrict $ Bi.encode o]

-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data ProxySecretKey w = ProxySecretKey
    { pskOmega    :: w
    , pskIssuerPk :: PublicKey
    , pskCert     :: ProxyCert w
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySecretKey w)
instance Hashable w => Hashable (ProxySecretKey w)

instance (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySecretKey w) where
    build (ProxySecretKey w iPk _) =
        bprint ("ProxySk { w = "%build%", iPk = "%build%" }") w iPk

deriveSafeCopySimple 0 'base ''ProxySecretKey

-- | Creates proxy secret key
createProxySecretKey :: (Bi w) => SecretKey -> PublicKey -> w -> ProxySecretKey w
createProxySecretKey issuerSk delegatePk w =
    ProxySecretKey w (toPublic issuerSk) $ createProxyCert issuerSk delegatePk w


-- | Delegate signature made with certificate-based permission. @a@
-- stays for message type used in proxy (ω in the implementation
-- notes), @b@ for type of message signed.
data ProxySignature w a = ProxySignature
    { pdOmega      :: w
    , pdDelegatePk :: PublicKey
    , pdCert       :: ProxyCert w
    , pdSig        :: Ed25519.Signature
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySignature w a)
instance Hashable w => Hashable (ProxySignature w a)

instance (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySignature w a) where
    build ProxySignature{..} =
        bprint ("Proxy signature { w = "%build%", delegatePk = "%build%" }")
               pdOmega pdDelegatePk

instance (SafeCopy w) => SafeCopy (ProxySignature w a) where
    putCopy ProxySignature{..} = contain $ do
        safePut pdOmega
        safePut pdDelegatePk
        safePut pdCert
        safePut pdSig
    getCopy = contain $
        ProxySignature <$> safeGet <*> safeGet <*> safeGet <*> safeGet

-- | Make a proxy delegate signature with help of certificate.
proxySign
    :: (Bi a)
    => SecretKey -> ProxySecretKey w -> a -> ProxySignature w a
proxySign sk@(SecretKey delegateSk) (ProxySecretKey o (PublicKey issuerPk) cert) m =
    ProxySignature
    { pdOmega = o
    , pdDelegatePk = toPublic sk
    , pdCert = cert
    , pdSig = sigma
    }
  where
    sigma =
        Ed25519.dsign delegateSk $
        mconcat ["01", Ed25519.openPublicKey issuerPk, BSL.toStrict $ Bi.encode m]

-- CHECK: @proxyVerify
-- | Verify delegated signature given issuer's pk, signature, message
-- space predicate and message itself.
proxyVerify
    :: (Bi w, Bi a)
    => PublicKey -> ProxySignature w a -> (w -> Bool) -> a -> Bool
proxyVerify (PublicKey issuerPk) ProxySignature {..} omegaPred m =
    and [predCorrect, certValid, sigValid]
  where
    PublicKey pdDelegatePkRaw = pdDelegatePk
    predCorrect = omegaPred pdOmega
    certValid =
        Ed25519.dverify
            issuerPk
            (mconcat
                 [ "00"
                 , Ed25519.openPublicKey pdDelegatePkRaw
                 , BSL.toStrict $ Bi.encode pdOmega
                 ])
            (unProxyCert pdCert)
    sigValid =
        Ed25519.dverify
            pdDelegatePkRaw
            (mconcat
                 [ "01"
                 , Ed25519.openPublicKey issuerPk
                 , BSL.toStrict $ Bi.encode m
                 ])
            pdSig

-- CHECK: @checkProxySecretKey
-- | Checks if proxy secret key is consistent and is related to
-- secretKey passed.
checkProxySecretKey :: (Bi w) => SecretKey -> ProxySecretKey w -> Bool
checkProxySecretKey delegateSk pSk@ProxySecretKey{..} =
    proxyVerify pskIssuerPk sig (const True) dummyData
  where
    dummyData :: ByteString
    dummyData = "nakshtalt"
    sig = proxySign delegateSk pSk dummyData
