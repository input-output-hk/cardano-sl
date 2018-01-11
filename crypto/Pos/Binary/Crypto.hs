{-# LANGUAGE CPP          #-}
{-# LANGUAGE ViewPatterns #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import qualified Crypto.ECC.Edwards25519 as Ed25519
import           Crypto.Hash (digestFromByteString)
import qualified Crypto.PVSS as Pvss
import qualified Crypto.SCRAPE as Scrape
import qualified Crypto.Sign.Ed25519 as EdStandard
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Formatting (int, sformat, (%))

import           Pos.Binary.Class (AsBinary (..), Bi (..), Cons (..), Field (..), decodeBinary,
                                   deriveSimpleBi, encodeBinary, encodeListLen, enforceSize)
import           Pos.Crypto.AsBinary (decShareBytes, encShareBytes, secretBytes, vssPublicKeyBytes)
import           Pos.Crypto.Configuration (HasCryptoConfiguration)
import           Pos.Crypto.Hashing (AbstractHash (..), HashAlgorithm, WithHash (..), withHash)
import           Pos.Crypto.HD (HDAddressPayload (..))
import           Pos.Crypto.Scrypt (EncryptedPass (..))
import qualified Pos.Crypto.SecretSharing as C
import           Pos.Crypto.Signing.Check (validateProxySecretKey)
import           Pos.Crypto.Signing.Types (ProxyCert (..), ProxySecretKey (..), ProxySignature (..),
                                           PublicKey (..), SecretKey (..), Signature (..),
                                           Signed (..))
import           Pos.Crypto.Signing.Types.Redeem (RedeemPublicKey (..), RedeemSecretKey (..),
                                                  RedeemSignature (..))
import           Pos.Crypto.Signing.Types.Safe (EncryptedSecretKey (..), PassPhrase,
                                                passphraseLength)

instance Bi a => Bi (WithHash a) where
    encode = encode . whData
    decode = withHash <$> decode

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance (Typeable algo, Typeable a, HashAlgorithm algo) => Bi (AbstractHash algo a) where
    encode (AbstractHash digest) = encode (ByteArray.convert digest :: ByteString)
    decode = do
        bs <- decode @ByteString
        case digestFromByteString bs of
            Nothing -> fail "AbstractHash.decode: invalid digest"
            Just x  -> pure (AbstractHash x)

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

#define BiPvss(T, PT) \
  instance Bi T where {\
    encode = encodeBinary ;\
    decode = decodeBinary };\
  deriving instance Bi PT ;\

BiPvss (Scrape.PublicKey, C.VssPublicKey)
BiPvss (Scrape.KeyPair, C.VssKeyPair)
BiPvss (Scrape.Secret, C.Secret)
BiPvss (Scrape.DecryptedShare, C.DecShare)
BiPvss (Scrape.EncryptedSi, C.EncShare)

instance Bi Scrape.ExtraGen where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.Commitment where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.Proof where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Scrape.ParallelProofs where
    encode = encodeBinary
    decode = decodeBinary

deriveSimpleBi ''C.SecretProof [
    Cons 'C.SecretProof [
        Field [| C.spExtraGen       :: Scrape.ExtraGen       |],
        Field [| C.spProof          :: Scrape.Proof          |],
        Field [| C.spParallelProofs :: Scrape.ParallelProofs |],
        Field [| C.spCommitments    :: [Scrape.Commitment]   |]
    ]
  ]

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

-- !A note about these instances! --
--
-- For most of the secret sharing types the only check we do during
-- deserialization is length check. As long as length matches our
-- expectations, the decoding succeeds (look at 'Binary' instances in
-- 'pvss') which in turn means that we can use 'fromBinary' and be quite
-- sure it will succeed. That's why it's important to check length here
-- (this check is cheap, so it's good to do it as soon as possible).
-- 'SecretProof' used to be an exception, but currently we don't use
-- 'AsBinary' for 'SecretProof' (we might in the future); this said, it's
-- alright to use 'AsBinary' for variable-length things as long as you're
-- careful.
--
#define BiMacro(B, BYTES) \
  instance Bi (AsBinary B) where {\
    encode (AsBinary bs) = encode bs ;\
    decode = do { bs <- decode \
                ; when (BYTES /= length bs) (fail $ "AsBinary B: length mismatch!") \
                ; return (AsBinary bs) } }; \

BiMacro(C.VssPublicKey, vssPublicKeyBytes)
BiMacro(C.Secret, secretBytes)
BiMacro(C.DecShare, decShareBytes)
BiMacro(C.EncShare, encShareBytes)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

instance Bi Ed25519.PointCompressed where
    encode (Ed25519.unPointCompressed -> k) = encode k
    decode = Ed25519.pointCompressed <$> decode

instance Bi Ed25519.Scalar where
    encode (Ed25519.unScalar -> k) = encode k
    decode = Ed25519.scalar <$> decode

instance Bi Ed25519.Signature where
    encode (Ed25519.Signature s) = encode s
    decode = Ed25519.Signature <$> decode

instance Bi CC.ChainCode where
    encode (CC.ChainCode c) = encode c
    decode = CC.ChainCode <$> decode

instance Bi CC.XPub where
    encode (CC.unXPub -> kc) = encode kc
    decode = either fail pure . CC.xpub =<< decode

instance Bi CC.XPrv where
    encode (CC.unXPrv -> kc) = encode kc
    decode = either fail pure . CC.xprv =<< decode @ByteString

instance Bi CC.XSignature where
    encode (CC.unXSignature -> bs) = encode bs
    decode = either fail pure . CC.xsignature =<< decode

deriving instance Typeable a => Bi (Signature a)
deriving instance Bi PublicKey
deriving instance Bi SecretKey

instance Bi EncryptedSecretKey where
    encode (EncryptedSecretKey sk pph) = encodeListLen 2
                                      <> encode sk
                                      <> encode pph
    decode = EncryptedSecretKey
         <$  enforceSize "EncryptedSecretKey" 2
         <*> decode
         <*> decode

instance Bi a => Bi (Signed a) where
    encode (Signed v s) = encodeListLen 2
                       <> encode v
                       <> encode s
    decode = Signed
         <$  enforceSize "Signed" 2
         <*> decode
         <*> decode

deriving instance Typeable w => Bi (ProxyCert w)

instance (Bi w, HasCryptoConfiguration) => Bi (ProxySecretKey w) where
    encode UnsafeProxySecretKey{..} =
        encodeListLen 4
        <> encode pskOmega
        <> encode pskIssuerPk
        <> encode pskDelegatePk
        <> encode pskCert
    decode = do
        enforceSize "ProxySecretKey" 4
        pskOmega      <- decode
        pskIssuerPk   <- decode
        pskDelegatePk <- decode
        pskCert       <- decode
        case validateProxySecretKey UnsafeProxySecretKey{..} of
            Left err  -> fail $ toString ("decode@ProxySecretKey: " <> err)
            Right psk -> pure psk

instance (Typeable a, Bi w, HasCryptoConfiguration) =>
         Bi (ProxySignature w a) where
    encode ProxySignature{..} = encodeListLen 2
                             <> encode psigPsk
                             <> encode psigSig
    decode = ProxySignature
          <$  enforceSize "ProxySignature" 2
          <*> decode
          <*> decode

instance Bi PassPhrase where
    encode pp = encode (ByteArray.convert pp :: ByteString)
    decode = do
        bs <- decode @ByteString
        let bl = BS.length bs
        -- Currently passphrase may be either 32-byte long or empty (for
        -- unencrypted keys).
        if bl == 0 || bl == passphraseLength
            then pure $ ByteArray.convert bs
            else fail . toString $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl

instance Bi EncryptedPass where
    encode (EncryptedPass ep) = encode ep
    decode = EncryptedPass <$> decode

-------------------------------------------------------------------------------
-- Hierarchical derivation
-------------------------------------------------------------------------------

instance Bi HDAddressPayload where
    encode (HDAddressPayload payload) = encode payload
    decode = HDAddressPayload <$> decode

-------------------------------------------------------------------------------
-- Standard Ed25519 instances for ADA redeem keys
-------------------------------------------------------------------------------

instance Bi EdStandard.PublicKey where
    encode (EdStandard.PublicKey k) = encode k
    decode = EdStandard.PublicKey <$> decode

instance Bi EdStandard.SecretKey where
    encode (EdStandard.SecretKey k) = encode k
    decode = EdStandard.SecretKey <$> decode

instance Bi EdStandard.Signature where
    encode (EdStandard.Signature s) = encode s
    decode = EdStandard.Signature <$> decode

deriving instance Bi RedeemPublicKey
deriving instance Bi RedeemSecretKey
deriving instance Typeable a => Bi (RedeemSignature a)
