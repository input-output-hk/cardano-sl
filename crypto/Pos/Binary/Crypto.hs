{-# LANGUAGE CPP #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet as CC
import           Control.Lens (_Left)
import           Crypto.Hash (digestFromByteString)
import qualified Crypto.PVSS as Pvss
import qualified Crypto.SCRAPE as Scrape
import qualified Crypto.Sign.Ed25519 as EdStandard
import qualified Data.ByteArray as ByteArray
import qualified Data.ByteString as BS
import           Formatting (int, sformat, (%))


import qualified Codec.CBOR.Decoding as D
import qualified Codec.CBOR.Encoding as E
import           Pos.Binary.Class (AsBinary (..), Bi (..), Cons (..), Field (..), decodeBinary,
                                   deriveSimpleBi, encodeBinary, encodeListLen, enforceSize)
import           Pos.Crypto.AsBinary (decShareBytes, encShareBytes, secretBytes, vssPublicKeyBytes)
import           Pos.Crypto.Hashing (AbstractHash (..), HashAlgorithm, WithHash (..), withHash)
import           Pos.Crypto.HD (HDAddressPayload (..))
import           Pos.Crypto.Scrypt (EncryptedPass (..))
import qualified Pos.Crypto.SecretSharing as C
import           Pos.Crypto.Signing.Types (ProxyCert (..), ProxySecretKey (..), ProxySignature (..),
                                           PublicKey (..), SecretKey (..), Signature (..),
                                           Signed (..))
import           Pos.Crypto.Signing.Types.Redeem (RedeemPublicKey (..), RedeemSecretKey (..),
                                                  RedeemSignature (..))
import           Pos.Crypto.Signing.Types.Safe (EncryptedSecretKey (..), PassPhrase,
                                                passphraseLength)
import           Pos.Util.Util (cborError, toCborError)

instance Bi a => Bi (WithHash a) where
    encode = encode . whData
    decode = withHash <$> decode

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance (Typeable algo, Typeable a, HashAlgorithm algo) => Bi (AbstractHash algo a) where
    encode (AbstractHash digest) = encode (ByteArray.convert digest :: BS.ByteString)
    -- FIXME bad decode: it reads an arbitrary-length byte string.
    -- Better instance: know the hash algorithm up front, read exactly that
    -- many bytes, fail otherwise. Then convert to a digest.
    decode = do
        bs <- decode @ByteString
        toCborError $ case digestFromByteString bs of
            Nothing -> Left "AbstractHash.decode: invalid digest"
            Just x  -> Right (AbstractHash x)

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

instance Bi Scrape.PublicKey where
    encode = encodeBinary
    decode = decodeBinary

deriving instance Bi C.VssPublicKey

instance Bi Scrape.KeyPair where
    encode = encodeBinary
    decode = decodeBinary

deriving instance Bi C.VssKeyPair

instance Bi Scrape.Secret where
    encode = encodeBinary
    decode = decodeBinary

deriving instance Bi C.Secret

instance Bi Scrape.DecryptedShare where
    encode = encodeBinary
    decode = decodeBinary

deriving instance Bi C.DecShare

instance Bi Scrape.EncryptedSi where
    encode = encodeBinary
    decode = decodeBinary

deriving instance Bi C.EncShare

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
                ; when (BYTES /= length bs) (cborError "AsBinary B: length mismatch!") \
                ; return (AsBinary bs) } }; \

BiMacro(C.VssPublicKey, vssPublicKeyBytes)
BiMacro(C.Secret, secretBytes)
BiMacro(C.DecShare, decShareBytes)
BiMacro(C.EncShare, encShareBytes)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

encodeXSignature :: CC.XSignature -> E.Encoding
encodeXSignature a = encode $ CC.unXSignature a

decodeXSignature :: D.Decoder s CC.XSignature
decodeXSignature = toCborError . over _Left fromString . CC.xsignature =<< decode

instance Typeable a => Bi (Signature a) where
    encode (Signature a) = encodeXSignature a
    decode = fmap Signature decodeXSignature

encodeXPub :: CC.XPub -> E.Encoding
encodeXPub a = encode $ CC.unXPub a

decodeXPub :: D.Decoder s CC.XPub
decodeXPub = toCborError . over _Left fromString . CC.xpub =<< decode

instance Bi PublicKey where
    encode (PublicKey a) = encodeXPub a
    decode = fmap PublicKey decodeXPub

encodeXPrv :: CC.XPrv -> E.Encoding
encodeXPrv a = encode $ CC.unXPrv a

decodeXPrv :: D.Decoder s CC.XPrv
decodeXPrv = toCborError . over _Left fromString . CC.xprv =<< decode @ByteString

instance Bi SecretKey where
    encode (SecretKey a) = encodeXPrv a
    decode = fmap SecretKey decodeXPrv

instance Bi EncryptedSecretKey where
    encode (EncryptedSecretKey sk pph) = encodeListLen 2
                                      <> encodeXPrv sk
                                      <> encode pph
    decode = EncryptedSecretKey
         <$  enforceSize "EncryptedSecretKey" 2
         <*> decodeXPrv
         <*> decode

instance Bi a => Bi (Signed a) where
    encode (Signed v s) = encodeListLen 2
                       <> encode v
                       <> encode s
    decode = Signed
         <$  enforceSize "Signed" 2
         <*> decode
         <*> decode

instance Typeable w => Bi (ProxyCert w) where
    encode (ProxyCert a) = encodeXSignature a
    decode = fmap ProxyCert decodeXSignature

instance Bi w => Bi (ProxySecretKey w) where
    encode UncheckedProxySecretKey{..} =
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
        pure UncheckedProxySecretKey {..}

instance (Typeable a, Bi w) =>
         Bi (ProxySignature w a) where
    encode ProxySignature{..} = encodeListLen 2
                             <> encode psigPsk
                             <> encodeXSignature psigSig
    decode = ProxySignature
          <$  enforceSize "ProxySignature" 2
          <*> decode
          <*> decodeXSignature

instance Bi PassPhrase where
    encode pp = encode (ByteArray.convert pp :: ByteString)
    decode = do
        bs <- decode @ByteString
        let bl = BS.length bs
        -- Currently passphrase may be either 32-byte long or empty (for
        -- unencrypted keys).
        toCborError $ if bl == 0 || bl == passphraseLength
            then Right $ ByteArray.convert bs
            else Left $ sformat
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
