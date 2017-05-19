{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet    as CC
import qualified Crypto.ECC.Edwards25519  as Ed25519
import           Crypto.Hash              (digestFromByteString, hashDigestSize)
import qualified Crypto.PVSS              as Pvss
import qualified Crypto.Sign.Ed25519      as EdStandard
import qualified Data.Binary              as Binary
import           Data.Binary.Get          (getByteString, label)
import           Data.Binary.Put          (putByteString)
import qualified Data.ByteArray           as ByteArray
import qualified Data.ByteString          as BS
import           Data.SafeCopy            (SafeCopy (..))
import           Formatting               (int, sformat, stext, (%))

import           Pos.Binary.Class         (AsBinary (..), Bi (..), getCopyBi, putCopyBi)
import           Pos.Crypto.Hashing       (AbstractHash (..), Hash, HashAlgorithm,
                                           WithHash (..), withHash)
import           Pos.Crypto.HD            (HDAddressPayload (..))
import           Pos.Crypto.RedeemSigning (RedeemPublicKey (..), RedeemSecretKey (..),
                                           RedeemSignature (..))
import           Pos.Crypto.SafeSigning   (EncryptedSecretKey (..), PassPhrase)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssKeyPair (..), VssPublicKey (..))
import           Pos.Crypto.Signing       (ProxyCert (..), ProxySecretKey (..),
                                           ProxySignature (..), PublicKey (..),
                                           SecretKey (..), Signature (..), Signed (..))

instance Bi a => Bi (WithHash a) where
    put = put . whData
    get = withHash <$> get

instance Bi a => SafeCopy (WithHash a) where
    putCopy = putCopyBi
    getCopy = getCopyBi "WithHash"

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashAlgorithm algo => Bi (AbstractHash algo a) where
    {-# SPECIALIZE instance Bi (Hash a) #-}
    get = label "AbstractHash" $ do
        bs <- fmap BS.copy $ getByteString $ hashDigestSize @algo $
              error "Pos.Crypto.Hashing.get: HashAlgorithm value is evaluated!"
        case digestFromByteString bs of
            -- It's impossible because getByteString will already fail if
            -- there weren't enough bytes available
            Nothing -> fail "Pos.Crypto.Hashing.get: impossible"
            Just x  -> return (AbstractHash x)
    put (AbstractHash h) =
        putByteString (ByteArray.convert h)

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

#define BiPvss(T, PT) \
  instance Bi T where {\
    put = Binary.put ;\
    get = label "T" Binary.get }; \
  deriving instance Bi PT ;\

BiPvss (Pvss.PublicKey, VssPublicKey)
BiPvss (Pvss.KeyPair, VssKeyPair)
BiPvss (Pvss.Secret, Secret)
BiPvss (Pvss.DecryptedShare, Share)
BiPvss (Pvss.EncryptedShare, EncShare)
BiPvss (Pvss.Proof, SecretProof)

instance Binary.Binary SecretSharingExtra
instance Bi SecretSharingExtra where
    put = Binary.put
    get = Binary.get

deriving instance Bi (AsBinary SecretSharingExtra)

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

#define BiMacro(B, Bytes) \
  instance Bi (AsBinary B) where {\
    put (AsBinary bs) = putByteString bs ;\
    get = label "B (Bytes bytes)" $ AsBinary <$> getByteString Bytes}; \

BiMacro(VssPublicKey, 33)
BiMacro(Secret, 33)
BiMacro(Share, 101) --4+33+64
BiMacro(EncShare, 101)
BiMacro(SecretProof, 64)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

secretKeyLength, publicKeyLength, signatureLength, chainCodeLength,
    encryptedKeyLength, passphraseLength :: Int
secretKeyLength = 32
publicKeyLength = 32
encryptedKeyLength = 96
signatureLength = 64
chainCodeLength = 32
passphraseLength = 32

putAssertLength :: Monad m => Text -> Int -> ByteString -> m ()
putAssertLength typeName expectedLength bs =
    when (BS.length bs /= expectedLength) $ error $
        sformat ("put@"%stext%": expected length "%int%", not "%int)
                typeName expectedLength (BS.length bs)

instance Bi Ed25519.PointCompressed where
    put (Ed25519.unPointCompressed -> k) = do
        putAssertLength "PointCompressed" publicKeyLength k
        putByteString k
    get = label "Ed25519.PointCompressed" $
        Ed25519.pointCompressed <$> getByteString publicKeyLength

instance Bi Ed25519.Scalar where
    put (Ed25519.unScalar -> k) = do
        putAssertLength "Scalar" secretKeyLength k
        putByteString k
    get = label "Ed25519.Scalar" $
        Ed25519.scalar <$> getByteString secretKeyLength

instance Bi Ed25519.Signature where
    put (Ed25519.Signature s) = do
        putAssertLength "Signature" signatureLength s
        putByteString s
    get = label "Ed25519.Signature" $
        Ed25519.Signature <$> getByteString signatureLength

instance Bi CC.ChainCode where
    put (CC.ChainCode c) = do
        putAssertLength "ChainCode" chainCodeLength c
        putByteString c
    get = label "CC.ChainCode" $
        CC.ChainCode <$> getByteString chainCodeLength

instance Bi CC.XPub where
    put (CC.unXPub -> kc) = do
        putAssertLength "XPub" (publicKeyLength + chainCodeLength) kc
        putByteString kc
    get = label "CC.XPub" $
        getByteString (publicKeyLength + chainCodeLength) >>=
        either fail pure . CC.xpub

instance Bi CC.XPrv where
    put (CC.unXPrv -> kc) = do
        putAssertLength "XPrv" encryptedKeyLength kc
        putByteString kc
    get = label "CC.XPrv" $
        getByteString encryptedKeyLength >>=
        either fail pure . CC.xprv

instance Bi CC.XSignature where
    put (CC.unXSignature -> bs) = do
        putAssertLength "XSignature" signatureLength bs
        putByteString bs
    get = label "CC.XSignature" $
        getByteString signatureLength >>=
        either fail pure . CC.xsignature

deriving instance Bi (Signature a)
deriving instance Bi PublicKey
deriving instance Bi SecretKey

instance Bi EncryptedSecretKey where
    put (EncryptedSecretKey sk pph) = put sk >> put pph
    get = label "EncryptedSecretKey" $ liftM2 EncryptedSecretKey get get

instance Bi a => Bi (Signed a) where
    put (Signed v s) = put (v,s)
    get = label "Signed" $ Signed <$> get <*> get

deriving instance Bi (ProxyCert w)

instance (Bi w) => Bi (ProxySecretKey w) where
    put (ProxySecretKey w iPk dPk cert) = put w >> put iPk >> put dPk >> put cert
    get = label "ProxySecretKey" $ liftM4 ProxySecretKey get get get get

instance (Bi w) => Bi (ProxySignature w a) where
    put ProxySignature{..} = do
        put pdOmega
        put pdDelegatePk
        put pdCert
        put pdSig
    get = label "ProxySignature" $ liftM4 ProxySignature get get get get

instance Bi PassPhrase where
    put pp = do
        -- currently passphrase may be 32-byte long, or empty
        -- (for unencrypted keys)
        let bs = BS.pack $ ByteArray.unpack pp
            bl = BS.length bs
        unless (bl `elem` [0, 32]) $ error $
            sformat ("put@PassPhrase: expected length 0 or 32, not "%int)
                bl
        putByteString bs
    get = label "PassPhrase" $
          ByteArray.pack . BS.unpack <$>
              (getByteString passphraseLength <|> getByteString 0)

-------------------------------------------------------------------------------
-- Hierarchical derivation
-------------------------------------------------------------------------------

instance Binary.Binary HDAddressPayload
instance Bi HDAddressPayload

-------------------------------------------------------------------------------
-- Standard Ed25519 instances for ADA redeem keys
-------------------------------------------------------------------------------

standardSecretKeyLength, standardPublicKeyLength, standardSignatureLength :: Int
standardSecretKeyLength = 64
standardPublicKeyLength = 32
standardSignatureLength = 64

instance Bi EdStandard.PublicKey where
    put (EdStandard.PublicKey k) = do
        putAssertLength "PublicKey" standardPublicKeyLength k
        putByteString k
    get = label "EdStandard.PublicKey" $
        EdStandard.PublicKey <$> getByteString standardPublicKeyLength

instance Bi EdStandard.SecretKey where
    put (EdStandard.SecretKey k) = do
        putAssertLength "SecretKey" standardSecretKeyLength k
        putByteString k
    get = label "EdStandard.SecretKey" $
        EdStandard.SecretKey <$> getByteString standardSecretKeyLength

instance Bi EdStandard.Signature where
    put (EdStandard.Signature s) = do
        putAssertLength "Signature" standardSignatureLength s
        putByteString s
    get = label "EdStandard.Signature" $
        EdStandard.Signature <$> getByteString standardSignatureLength

deriving instance Bi RedeemPublicKey
deriving instance Bi RedeemSecretKey
deriving instance Bi (RedeemSignature a)
