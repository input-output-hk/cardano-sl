{-# LANGUAGE TemplateHaskell #-}

-- | Signing done with public/private keys.
module Pos.Crypto.Signing
       (
       -- * Keys
         PublicKey (..)
       , SecretKey (..)
       , emptyPass
       , keyGen
       , deterministicKeyGen
       , toPublic
       , formatFullPublicKey
       , fullPublicKeyF
       , fullPublicKeyHexF
       , parseFullPublicKey

       -- * Signing and verification
       , Signature (..)
       , sign
       , checkSig
       , fullSignatureHexF

       , Signed (..)
       , mkSigned

       -- * Versions for raw bytestrings
       , signRaw
       , verifyRaw

       -- * Proxy signature scheme
       , ProxyCert (..)
       , verifyProxyCert
       , ProxySecretKey (..)
       , verifyProxySecretKey
       , ProxySignature (..)
       , proxySign
       , proxyVerify
       ) where

import qualified Cardano.Crypto.Wallet  as CC
-- import qualified Cardano.Crypto.Wallet.Encrypted as CC
-- import qualified Crypto.ECC.Edwards25519         as Ed25519
import           Data.ByteArray         (ScrubbedBytes)
import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Coerce            (coerce)
import           Data.Hashable          (Hashable)
import qualified Data.Text.Buildable    as B
import           Data.Text.Lazy.Builder (Builder)
import           Formatting             (Format, bprint, build, fitLeft, later, (%), (%.))
import           Prelude                (show)
import qualified Serokell.Util.Base16   as B16
import qualified Serokell.Util.Base64   as Base64 (decode, formatBase64)
import           Serokell.Util.Text     (pairF)
import           Universum              hiding (show)

import           Pos.Binary.Class       (Bi, Raw)
import qualified Pos.Binary.Class       as Bi
import           Pos.Crypto.Hashing     (hash)
import           Pos.Crypto.Random      (secureRandomBS)
import           Pos.Crypto.SignTag     (SignTag (SignProxySK), signTag)

----------------------------------------------------------------------------
-- Keys, key generation & printing & decoding
----------------------------------------------------------------------------

-- | Wrapper around 'CC.XPub'.
newtype PublicKey = PublicKey CC.XPub
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

-- | Wrapper around 'CC.XPrv'.
newtype SecretKey = SecretKey CC.XPrv
    deriving (NFData)

-- | Generate a public key from a secret key. Fast (it just drops some bytes
-- off the secret key).
toPublic :: SecretKey -> PublicKey
toPublic (SecretKey k) = PublicKey (CC.toXPub k)

-- | Direct comparison of secret keys is a security issue (cc @vincent)
instance Bi SecretKey => Eq SecretKey where
    a == b = hash a == hash b

instance Show SecretKey where
    show sk = "<secret of " ++ show (toPublic sk) ++ ">"

instance Bi PublicKey => B.Buildable PublicKey where
    build = bprint ("pub:"%shortPublicKeyHexF)

instance Bi PublicKey => B.Buildable SecretKey where
    build = bprint ("sec:"%shortPublicKeyHexF) . toPublic

-- | 'Builder' for 'PublicKey' to show it in base64 encoded form.
formatFullPublicKey :: PublicKey -> Builder
formatFullPublicKey (PublicKey pk) =
    Base64.formatBase64 . CC.unXPub $ pk

-- | Formatter for 'PublicKey' to show it in base64.
fullPublicKeyF :: Format r (PublicKey -> r)
fullPublicKeyF = later formatFullPublicKey

-- | Formatter for 'PublicKey' to show it in hex.
fullPublicKeyHexF :: Format r (PublicKey -> r)
fullPublicKeyHexF = later $ \(PublicKey x) -> B16.formatBase16 . CC.unXPub $ x

-- | Formatter for 'PublicKey' to show it in hex, but only first 8 chars.
shortPublicKeyHexF :: Format r (PublicKey -> r)
shortPublicKeyHexF = fitLeft 8 %. fullPublicKeyHexF

-- | Parse 'PublicKey' from base64 encoded string.
parseFullPublicKey :: (Bi PublicKey) => Text -> Maybe PublicKey
parseFullPublicKey s = do
    b <- rightToMaybe $ Base64.decode s
    (unconsumed, _, a) <- rightToMaybe $ Bi.decodeOrFail (BSL.fromStrict b)
    guard $ BSL.null unconsumed
    pure a

emptyPass :: ScrubbedBytes
emptyPass = mempty

-- TODO: this is just a placeholder for actual (not ready yet) derivation
-- of keypair from seed in cardano-crypto API
createKeypairFromSeed :: BS.ByteString -> Maybe (CC.XPub, CC.XPrv)
createKeypairFromSeed seed = do
    prv <- CC.generate seed emptyPass
    return (CC.toXPub prv, prv)

-- | Generate a key pair.
keyGen :: MonadIO m => m (PublicKey, SecretKey)
keyGen = liftIO $ do
    seed <- secureRandomBS 32
    case createKeypairFromSeed seed of
        Nothing -> error "Pos.Crypto.Signing.keyGen:\
                         \ createKeypairFromSeed_ failed"
        Just (pk, sk) -> return (PublicKey pk, SecretKey sk)

-- | Create key pair deterministically from 32 bytes.
deterministicKeyGen :: BS.ByteString -> Maybe (PublicKey, SecretKey)
deterministicKeyGen seed =
    bimap PublicKey SecretKey <$> createKeypairFromSeed seed

----------------------------------------------------------------------------
-- Signatures
----------------------------------------------------------------------------

-- | Wrapper around 'CC.XSignature'.
newtype Signature a = Signature CC.XSignature
    deriving (Eq, Ord, Show, Generic, NFData, Hashable, Typeable)

instance B.Buildable (Signature a) where
    build _ = "<signature>"

-- | Formatter for 'Signature' to show it in hex.
fullSignatureHexF :: Format r (Signature a -> r)
fullSignatureHexF = later $ \(Signature x) ->
    B16.formatBase16 . CC.unXSignature $ x

-- | Encode something with 'Binary' and sign it.
sign
    :: Bi a
    => SignTag         -- ^ See docs for 'SignTag'
    -> SecretKey
    -> a
    -> Signature a
sign t k = coerce . signRaw (Just t) k . Bi.encodeStrict

-- | Sign a bytestring.
signRaw
    :: Maybe SignTag   -- ^ See docs for 'SignTag'. Unlike in 'sign', we
                       -- allow no tag to be provided just in case you need
                       -- to sign /exactly/ the bytestring you provided
    -> SecretKey
    -> ByteString
    -> Signature Raw
signRaw mbTag (SecretKey k) x = Signature (CC.sign emptyPass k (tag <> x))
  where
    tag = maybe mempty signTag mbTag

-- CHECK: @checkSig
-- | Verify a signature.
-- #verifyRaw
checkSig :: Bi a => SignTag -> PublicKey -> a -> Signature a -> Bool
checkSig t k x s = verifyRaw (Just t) k (Bi.encodeStrict x) (coerce s)

-- CHECK: @verifyRaw
-- | Verify raw 'ByteString'.
verifyRaw :: Maybe SignTag -> PublicKey -> ByteString -> Signature Raw -> Bool
verifyRaw mbTag (PublicKey k) x (Signature s) = CC.verify k (tag <> x) s
  where
    tag = maybe mempty signTag mbTag

-- | Value and signature for this value.
data Signed a = Signed
    { signedValue :: !a              -- ^ Value to be signed
    , signedSig   :: !(Signature a)  -- ^ 'Signature' of 'signedValue'
    } deriving (Show, Eq, Ord, Generic)

-- | Smart constructor for 'Signed' data type with proper signing.
mkSigned :: (Bi a) => SignTag -> SecretKey -> a -> Signed a
mkSigned t sk x = Signed x (sign t sk x)

----------------------------------------------------------------------------
-- Proxy signing
----------------------------------------------------------------------------

-- | Proxy certificate, made of ω + public key of delegate.
newtype ProxyCert w = ProxyCert { unProxyCert :: CC.XSignature }
    deriving (Eq, Ord, Show, Generic, NFData, Hashable)

instance B.Buildable (ProxyCert w) where
    build _ = "<proxy_cert>"

-- | Checks if certificate is valid, given issuer pk, delegate pk and ω.
verifyProxyCert :: (Bi w) => PublicKey -> PublicKey -> w -> ProxyCert w -> Bool
verifyProxyCert issuerPk (PublicKey delegatePk) o (ProxyCert sig) =
    checkSig SignProxySK issuerPk
        (mconcat ["00", CC.unXPub delegatePk, Bi.encodeStrict o])
        (Signature sig)

-- | Convenient wrapper for secret key, that's basically ω plus
-- certificate.
data ProxySecretKey w = ProxySecretKey
    { pskOmega      :: w
    , pskIssuerPk   :: PublicKey
    , pskDelegatePk :: PublicKey
    , pskCert       :: ProxyCert w
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySecretKey w)
instance Hashable w => Hashable (ProxySecretKey w)

instance {-# OVERLAPPABLE #-}
         (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySecretKey w) where
    build (ProxySecretKey w iPk dPk _) =
        bprint ("ProxySk { w = "%build%", iPk = "%build%", dPk = "%build%" }") w iPk dPk

instance (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySecretKey (w,w)) where
    build (ProxySecretKey w iPk dPk _) =
        bprint ("ProxySk { w = "%pairF%", iPk = "%build%", dPk = "%build%" }") w iPk dPk

-- | Checks if proxy secret key is valid (the signature/cert inside is
-- correct).
verifyProxySecretKey :: (Bi w) => ProxySecretKey w -> Bool
verifyProxySecretKey ProxySecretKey{..} =
    verifyProxyCert pskIssuerPk pskDelegatePk pskOmega pskCert

-- | Delegate signature made with certificate-based permission. @a@
-- stays for message type used in proxy (ω in the implementation
-- notes), @b@ for type of message signed.
data ProxySignature w a = ProxySignature
    { pdOmega      :: w
    , pdDelegatePk :: PublicKey
    , pdCert       :: ProxyCert w
    , pdSig        :: CC.XSignature
    } deriving (Eq, Ord, Show, Generic)

instance NFData w => NFData (ProxySignature w a)
instance Hashable w => Hashable (ProxySignature w a)

instance {-# OVERLAPPABLE #-}
         (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySignature w a) where
    build ProxySignature{..} =
        bprint ("Proxy signature { w = "%build%", delegatePk = "%build%" }")
               pdOmega pdDelegatePk

instance (B.Buildable w, Bi PublicKey) => B.Buildable (ProxySignature (w,w) a) where
    build ProxySignature{..} =
        bprint ("Proxy signature { w = "%pairF%", delegatePk = "%build%" }")
               pdOmega pdDelegatePk

-- | Make a proxy delegate signature with help of certificate. If the
-- delegate secret key passed doesn't pair with delegate public key in
-- certificate inside, we panic. Please check this condition outside
-- of this function.
proxySign
    :: (Bi a)
    => SignTag -> SecretKey -> ProxySecretKey w -> a -> ProxySignature w a
proxySign t sk@(SecretKey delegateSk) ProxySecretKey{..} m
    | toPublic sk /= pskDelegatePk =
        error "proxySign called with irrelevant certificate"
    | otherwise =
        ProxySignature
        { pdOmega = pskOmega
        , pdDelegatePk = pskDelegatePk
        , pdCert = pskCert
        , pdSig = sigma
        }
  where
    PublicKey issuerPk = pskIssuerPk
    sigma =
        CC.sign emptyPass delegateSk $
        mconcat
            -- it's safe to put the tag after issuerPk because `CC.unXPub
            -- issuerPk` always takes 64 bytes
            ["01", CC.unXPub issuerPk, signTag t, Bi.encodeStrict m]

-- CHECK: @proxyVerify
-- | Verify delegated signature given issuer's pk, signature, message
-- space predicate and message itself.
proxyVerify
    :: (Bi w, Bi a)
    => SignTag -> PublicKey -> ProxySignature w a -> (w -> Bool) -> a -> Bool
proxyVerify t iPk@(PublicKey issuerPk) ProxySignature{..} omegaPred m =
    and [predCorrect, certValid, sigValid]
  where
    PublicKey pdDelegatePkRaw = pdDelegatePk
    predCorrect = omegaPred pdOmega
    certValid = verifyProxyCert iPk pdDelegatePk pdOmega pdCert
    sigValid =
        CC.verify
            pdDelegatePkRaw
            (mconcat
                 [ "01"
                 , CC.unXPub issuerPk
                 , signTag t
                 , Bi.encodeStrict m
                 ])
            pdSig
