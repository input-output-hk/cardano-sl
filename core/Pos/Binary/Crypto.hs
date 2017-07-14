{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet      as CC
import qualified Crypto.ECC.Edwards25519    as Ed25519
import           Crypto.Hash                (digestFromByteString)
import qualified Crypto.PVSS                as Pvss
import qualified Crypto.Sign.Ed25519        as EdStandard
import qualified Data.ByteArray             as ByteArray
import qualified Data.ByteString            as BS
import           Data.SafeCopy              (SafeCopy (..))
import           Formatting                 (int, sformat, (%))

import           Pos.Binary.Class           (AsBinary (..), Bi (..), encodeBinary, decodeBinary, getCopyBi, putCopyBi,
                                            encodeListLen, enforceSize)
import           Pos.Crypto.Hashing         (AbstractHash (..), HashAlgorithm,
                                             WithHash (..), withHash)
import           Pos.Crypto.HD              (HDAddressPayload (..))
import           Pos.Crypto.RedeemSigning   (RedeemPublicKey (..), RedeemSecretKey (..),
                                             RedeemSignature (..))
import           Pos.Crypto.SafeSigning     (EncryptedSecretKey (..), PassPhrase)
import           Pos.Crypto.SecretSharing   (EncShare (..), Secret (..), SecretProof (..),
                                             SecretSharingExtra (..), Share (..),
                                             VssKeyPair (..), VssPublicKey (..))
import           Pos.Crypto.Signing         (ProxyCert (..), ProxySecretKey (..),
                                             ProxySignature (..), PublicKey (..),
                                             SecretKey (..), Signature (..), Signed (..))

instance Bi a => Bi (WithHash a) where
    encode = encode . whData
    decode = withHash <$> decode

instance (Typeable a, Bi a) => SafeCopy (WithHash a) where
    putCopy = putCopyBi
    getCopy = getCopyBi

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashAlgorithm algo => Bi (AbstractHash algo a) where
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

BiPvss (Pvss.PublicKey, VssPublicKey)
BiPvss (Pvss.KeyPair, VssKeyPair)
BiPvss (Pvss.Secret, Secret)
BiPvss (Pvss.DecryptedShare, Share)
BiPvss (Pvss.EncryptedShare, EncShare)
BiPvss (Pvss.Proof, SecretProof)

instance Bi Pvss.ExtraGen where
    encode = encodeBinary
    decode = decodeBinary

instance Bi Pvss.Commitment where
    encode = encodeBinary
    decode = decodeBinary

instance Bi SecretSharingExtra where
    encode (SecretSharingExtra eg comms) = encodeListLen 2
                                        <> encode eg
                                        <> encode comms
    decode = SecretSharingExtra
          <$  enforceSize "SecretSharingExtra" 2
          <*> decode
          <*> decode

deriving instance Bi (AsBinary SecretSharingExtra)

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

#define BiMacro(B) \
  instance Bi (AsBinary B) where {\
    encode (AsBinary bs) = encode bs ;\
    decode = AsBinary <$> decode}; \

BiMacro(VssPublicKey)
BiMacro(Secret)
BiMacro(Share)
BiMacro(EncShare)
BiMacro(SecretProof)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

passphraseLength :: Int
passphraseLength = 32

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

deriving instance Bi (Signature a)
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

deriving instance Bi (ProxyCert w)

instance Bi w => Bi (ProxySecretKey w) where
    encode ProxySecretKey{..} = encodeListLen 4
                             <> encode pskOmega
                             <> encode pskIssuerPk
                             <> encode pskDelegatePk
                             <> encode pskCert
    decode = ProxySecretKey <$  enforceSize "ProxySecretKey" 4
                            <*> decode
                            <*> decode
                            <*> decode
                            <*> decode

instance Bi w => Bi (ProxySignature w a) where
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
        -- Currently passphrase may be 32-byte long, or empty (for
        -- unencrypted keys).
        if bl == 0 || bl == passphraseLength
            then pure $ ByteArray.convert bs
            else fail . toString $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl

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
deriving instance Bi (RedeemSignature a)
