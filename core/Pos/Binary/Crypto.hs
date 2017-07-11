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
import qualified Data.Binary                as Binary
import qualified Data.ByteArray             as ByteArray
import qualified Data.ByteString            as BS
import qualified Data.ByteString.Lazy       as BSL
import           Data.Functor.Contravariant (contramap)
import           Data.SafeCopy              (SafeCopy (..))
import qualified Data.Store                 as Store
import qualified Data.Store.TH              as Store
import           Formatting                 (int, sformat, stext, (%))

import qualified Pos.Binary.Cbor            as Cbor
import           Pos.Binary.Class           (AsBinary (..), Bi (..), Size (..),
                                             StaticSize (..), getBytes, getCopyBi, label,
                                             labelP, labelS, putBytes, putCopyBi,
                                             putField, sizeOf)
import qualified Pos.Binary.Class           as Bi
import           Pos.Crypto.Hashing         (AbstractHash (..), HashAlgorithm,
                                             WithHash (..), hashDigestSize',
                                             reifyHashDigestSize, withHash, withHashCbor)
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
    size = contramap whData size
    put = put . whData
    get = withHash <$> get

instance Cbor.Bi a => Cbor.Bi (WithHash a) where
    encode = Cbor.encode . whData
    decode = withHashCbor <$> Cbor.decode

instance Bi a => SafeCopy (WithHash a) where
    putCopy = putCopyBi
    getCopy = getCopyBi "WithHash"

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashAlgorithm algo => Bi (AbstractHash algo a) where
    size = ConstSize (hashDigestSize' @algo)
    put (AbstractHash digest) = labelP "AbstractHash" $
        reifyHashDigestSize @algo (\(Proxy :: Proxy n) ->
            let bs = ByteArray.convert digest :: BS.ByteString
            in put (StaticSize @n bs))
    get =
        reifyHashDigestSize @algo (\(Proxy :: Proxy n) -> do
            sbs <- get
            let bs = unStaticSize @n sbs :: BS.ByteString
            case digestFromByteString bs of
                -- It's impossible because 'get' will already fail if there
                -- weren't enough bytes available
                Nothing -> error "AbstractHash.peek: impossible"
                Just x  -> pure (AbstractHash x))

instance HashAlgorithm algo => Cbor.Bi (AbstractHash algo a) where
    encode (AbstractHash digest) = Cbor.encode (ByteArray.convert digest :: ByteString)
    decode = do
        bs <- Cbor.decode @ByteString
        case digestFromByteString bs of
            Nothing -> fail "AbstractHash.decode: invalid digest"
            Just x  -> pure (AbstractHash x)

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

-- [CSL-1122] TODO: move elsewhere?
constantSizedBinaryToStoreGet :: Binary.Binary a => Int -> Store.Peek a
constantSizedBinaryToStoreGet bytes = do
    x <- getBytes bytes
    case Binary.decodeOrFail (BSL.fromStrict x) of
        Left (_, _, err) -> fail err
        Right (bs, _, res)
            | BSL.null bs -> pure res
            | otherwise   -> fail "unconsumed input"

-- [CSL-1122] TODO: more efficient 'put' and 'get' are possible if 'Store'
-- instances are added into pvss-haskell
#define BiPvss(T, PT, BYTES) \
  instance Bi T where {\
    size = ConstSize BYTES ;\
    put = labelP "T" . putBytes . BSL.toStrict . Binary.encode ;\
    get = label "T" $ constantSizedBinaryToStoreGet BYTES };\
  deriving instance Bi PT ;\

BiPvss (Pvss.PublicKey, VssPublicKey, 33)    -- yes it's 33 and not 32
BiPvss (Pvss.KeyPair, VssKeyPair, 65)        -- 32+33
BiPvss (Pvss.Secret, Secret, 33)
BiPvss (Pvss.DecryptedShare, Share, 101)     -- 4+33+64
BiPvss (Pvss.EncryptedShare, EncShare, 101)
BiPvss (Pvss.Proof, SecretProof, 64)

#define CborBiPvss(T, PT) \
  instance Cbor.Bi T where {\
    encode = Cbor.encodeBinary ;\
    decode = Cbor.decodeBinary };\
  deriving instance Cbor.Bi PT ;\

CborBiPvss (Pvss.PublicKey, VssPublicKey)
CborBiPvss (Pvss.KeyPair, VssKeyPair)
CborBiPvss (Pvss.Secret, Secret)
CborBiPvss (Pvss.DecryptedShare, Share)
CborBiPvss (Pvss.EncryptedShare, EncShare)
CborBiPvss (Pvss.Proof, SecretProof)

instance Store.Store Pvss.ExtraGen where
    size = ConstSize 33
    poke = labelP "Pvss.ExtraGen" . putBytes . BSL.toStrict . Binary.encode
    peek = label "Pvss.ExtraGen" $ constantSizedBinaryToStoreGet 33
instance Store.Store Pvss.Commitment where
    size = ConstSize 33
    poke = labelP "Pvss.Commitment" . putBytes . BSL.toStrict . Binary.encode
    peek = label "Pvss.Commitment" $ constantSizedBinaryToStoreGet 33

Store.makeStore ''SecretSharingExtra
instance Bi SecretSharingExtra where
    put = labelP "SecretSharingExtra" . Store.poke
    get = label "SecretSharingExtra" $ Store.peek
    size = Store.size

deriving instance Bi (AsBinary SecretSharingExtra)

instance Cbor.Bi SecretSharingExtra where
    encode = error "encode SecretSharingExtra"
    decode = error "decode SecretSharingExtra"

deriving instance Cbor.Bi (AsBinary SecretSharingExtra)

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

#define BiMacro(B, BYTES) \
  instance Bi (AsBinary B) where {\
    size = ConstSize BYTES ;\
    put (AsBinary bs) = putBytes bs ;\
    get = label "B (BYTES bytes)" $ AsBinary <$> getBytes BYTES}; \

BiMacro(VssPublicKey, 33)
BiMacro(Secret, 33)
BiMacro(Share, 101) --4+33+64
BiMacro(EncShare, 101)
BiMacro(SecretProof, 64)

#define CborBiMacro(B) \
  instance Cbor.Bi (AsBinary B) where {\
    encode (AsBinary bs) = Cbor.encode bs ;\
    decode = AsBinary <$> Cbor.decode}; \

CborBiMacro(VssPublicKey)
CborBiMacro(Secret)
CborBiMacro(Share)
CborBiMacro(EncShare)
CborBiMacro(SecretProof)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

secretKeyLength, publicKeyLength, signatureLength, chainCodeLength,
    encryptedKeyLength, passphraseLength :: Int
secretKeyLength = 32
publicKeyLength = 32
encryptedKeyLength = 128
signatureLength = 64
chainCodeLength = 32
passphraseLength = 32

putAssertLength :: Monad m => Text -> Int -> ByteString -> m ()
putAssertLength typeName expectedLength bs =
    when (BS.length bs /= expectedLength) $ error $
        sformat ("put@"%stext%": expected length "%int%", not "%int)
                typeName expectedLength (BS.length bs)

instance Bi Ed25519.PointCompressed where
    size = ConstSize publicKeyLength
    put (Ed25519.unPointCompressed -> k) = labelP "Ed25519.PointCompressed" $ do
        putAssertLength "PointCompressed" publicKeyLength k
        putBytes k
    get = label "Ed25519.PointCompressed" $
        Ed25519.pointCompressed <$> getBytes publicKeyLength

instance Cbor.Bi Ed25519.PointCompressed where
  encode (Ed25519.unPointCompressed -> k) = Cbor.encode k
  decode = Ed25519.pointCompressed <$> Cbor.decode

instance Bi Ed25519.Scalar where
    size = ConstSize secretKeyLength
    put (Ed25519.unScalar -> k) = labelP "Ed25519.Scalar" $ do
        putAssertLength "Scalar" secretKeyLength k
        putBytes k
    get = label "Ed25519.Scalar" $
        Ed25519.scalar <$> getBytes secretKeyLength

instance Cbor.Bi Ed25519.Scalar where
  encode (Ed25519.unScalar -> k) = Cbor.encode k
  decode = Ed25519.scalar <$> Cbor.decode

instance Bi Ed25519.Signature where
    size = ConstSize signatureLength
    put (Ed25519.Signature s) = labelP "Ed25519.Signature" $ do
        putAssertLength "Signature" signatureLength s
        putBytes s
    get = label "Ed25519.Signature" $
        Ed25519.Signature <$> getBytes signatureLength

instance Cbor.Bi Ed25519.Signature where
  encode (Ed25519.Signature s) = Cbor.encode s
  decode = Ed25519.Signature <$> Cbor.decode

instance Bi CC.ChainCode where
    size = ConstSize chainCodeLength
    put (CC.ChainCode c) = do
        putAssertLength "ChainCode" chainCodeLength c
        putBytes c
    get = label "CC.ChainCode" $
        CC.ChainCode <$> getBytes chainCodeLength

instance Cbor.Bi CC.ChainCode where
    encode (CC.ChainCode c) = Cbor.encode c
    decode = CC.ChainCode <$> Cbor.decode

instance Bi CC.XPub where
    size = ConstSize (publicKeyLength + chainCodeLength)
    put (CC.unXPub -> kc) = labelP "CC.XPub" $ do
        putAssertLength "XPub" (publicKeyLength + chainCodeLength) kc
        putBytes kc
    get = label "CC.XPub" $
        getBytes (publicKeyLength + chainCodeLength) >>=
        either fail pure . CC.xpub

instance Cbor.Bi CC.XPub where
    encode (CC.unXPub -> kc) = Cbor.encode kc
    decode = either fail pure . CC.xpub =<< Cbor.decode

instance Bi CC.XPrv where
    size = ConstSize encryptedKeyLength
    put (CC.unXPrv -> kc) = labelP "CC.XPrv" $ do
        putAssertLength "XPrv" encryptedKeyLength kc
        putBytes kc
    get = label "CC.XPrv" $
        getBytes encryptedKeyLength >>=
        either fail pure . CC.xprv

instance Cbor.Bi CC.XPrv where
    encode (CC.unXPrv -> kc) = Cbor.encode kc
    decode = either fail pure . CC.xprv =<< Cbor.decode @ByteString

instance Bi CC.XSignature where
    size = ConstSize signatureLength
    put (CC.unXSignature -> bs) = labelP "CC.XSignature" $ do
        putAssertLength "XSignature" signatureLength bs
        putBytes bs
    get = label "CC.XSignature" $
        getBytes signatureLength >>=
        either fail pure . CC.xsignature

instance Cbor.Bi CC.XSignature where
    encode (CC.unXSignature -> bs) = Cbor.encode bs
    decode = either fail pure . CC.xsignature =<< Cbor.decode

deriving instance Bi (Signature a)
deriving instance Bi PublicKey
deriving instance Bi SecretKey

deriving instance Cbor.Bi (Signature a)
deriving instance Cbor.Bi PublicKey
deriving instance Cbor.Bi SecretKey

instance Bi EncryptedSecretKey where
    size = Bi.combineSize (eskPayload, eskHash)
    put (EncryptedSecretKey sk pph) = put sk >> put pph
    get = label "EncryptedSecretKey" $ liftM2 EncryptedSecretKey get get

instance Cbor.Bi EncryptedSecretKey where
    encode (EncryptedSecretKey sk pph) = Cbor.encode sk <> Cbor.encode pph
    decode = EncryptedSecretKey <$> Cbor.decode <*> Cbor.decode

instance Bi a => Bi (Signed a) where
    size = Bi.combineSize (signedValue, signedSig)
    put (Signed v s) = put (v,s)
    get = label "Signed" $ Signed <$> get <*> get

instance Cbor.Bi a => Cbor.Bi (Signed a) where
    encode (Signed v s) = Cbor.encode v <> Cbor.encode s
    decode = Signed <$> Cbor.decode <*> Cbor.decode

deriving instance Bi (ProxyCert w)
deriving instance Cbor.Bi (ProxyCert w)

instance (Bi w) => Bi (ProxySecretKey w) where
    sizeNPut = labelS "ProxySecretKey" $
        putField pskOmega <>
        putField pskIssuerPk <>
        putField pskDelegatePk <>
        putField pskCert
    get = label "ProxySecretKey" $ liftM4 ProxySecretKey get get get get

instance Cbor.Bi w => Cbor.Bi (ProxySecretKey w) where
    encode ProxySecretKey{..} = Cbor.encode pskOmega
                             <> Cbor.encode pskIssuerPk
                             <> Cbor.encode pskDelegatePk
                             <> Cbor.encode pskCert
    decode = ProxySecretKey <$> Cbor.decode
                            <*> Cbor.decode
                            <*> Cbor.decode
                            <*> Cbor.decode

instance (Bi w) => Bi (ProxySignature w a) where
    sizeNPut = labelS "ProxySignature" $ putField psigPsk <> putField psigSig
    get = label "ProxySignature" $ liftM2 ProxySignature get get

instance Cbor.Bi w => Cbor.Bi (ProxySignature w a) where
    encode ProxySignature{..} = Cbor.encode psigPsk <> Cbor.encode psigSig
    decode = ProxySignature <$> Cbor.decode <*> Cbor.decode

instance Bi PassPhrase where
    size = ConstSize passphraseLength
    put pp = labelP "PassPhrase" $ do
        -- currently passphrase may be 32-byte long, or empty (for
        -- unencrypted keys). The empty passphrase is serialized as 32
        -- zeroes.
        let bs = ByteArray.convert pp
            bl = BS.length bs
        if | bl == 0 -> putBytes (BS.replicate passphraseLength 0)
           | bl == passphraseLength -> putBytes bs
           | otherwise -> error $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl
    get = let norm x | x == BS.replicate passphraseLength 0 = mempty
                     | otherwise                            = x
          in  label "PassPhrase" $
              ByteArray.convert . norm <$>
              getBytes passphraseLength

instance Cbor.Bi PassPhrase where
    encode pp = Cbor.encode (ByteArray.convert pp :: ByteString)
    decode = do
        bs <- Cbor.decode @ByteString
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
    size = sizeOf getHDAddressPayload
    put (HDAddressPayload payload) = labelP "HDAddressPayload" $ put payload
    get = label "HDAddressPayload" $ HDAddressPayload <$> get

instance Cbor.Bi HDAddressPayload where
    encode (HDAddressPayload payload) = Cbor.encode payload
    decode = HDAddressPayload <$> Cbor.decode

-------------------------------------------------------------------------------
-- Standard Ed25519 instances for ADA redeem keys
-------------------------------------------------------------------------------

standardSecretKeyLength, standardPublicKeyLength, standardSignatureLength :: Int
standardSecretKeyLength = 64
standardPublicKeyLength = 32
standardSignatureLength = 64

instance Bi EdStandard.PublicKey where
    size = ConstSize standardPublicKeyLength
    put (EdStandard.PublicKey k) = labelP "EdStandard.PublicKey" $ do
        putAssertLength "PublicKey" standardPublicKeyLength k
        putBytes k
    get = label "EdStandard.PublicKey" $
        EdStandard.PublicKey <$> getBytes standardPublicKeyLength

instance Cbor.Bi EdStandard.PublicKey where
    encode (EdStandard.PublicKey k) = Cbor.encode k
    decode = EdStandard.PublicKey <$> Cbor.decode

instance Bi EdStandard.SecretKey where
    size = ConstSize standardSecretKeyLength
    put (EdStandard.SecretKey k) = labelP "EdStandard.SecretKey" $ do
        putAssertLength "SecretKey" standardSecretKeyLength k
        putBytes k
    get = label "EdStandard.SecretKey" $
        EdStandard.SecretKey <$> getBytes standardSecretKeyLength

instance Cbor.Bi EdStandard.SecretKey where
    encode (EdStandard.SecretKey k) = Cbor.encode k
    decode = EdStandard.SecretKey <$> Cbor.decode

instance Bi EdStandard.Signature where
    size = ConstSize standardSignatureLength
    put (EdStandard.Signature s) = labelP "EdStandard.Signature" $ do
        putAssertLength "Signature" standardSignatureLength s
        putBytes s
    get = label "EdStandard.Signature" $
        EdStandard.Signature <$> getBytes standardSignatureLength

instance Cbor.Bi EdStandard.Signature where
    encode (EdStandard.Signature s) = Cbor.encode s
    decode = EdStandard.Signature <$> Cbor.decode

deriving instance Bi RedeemPublicKey
deriving instance Bi RedeemSecretKey
deriving instance Bi (RedeemSignature a)

deriving instance Cbor.Bi RedeemPublicKey
deriving instance Cbor.Bi RedeemSecretKey
deriving instance Cbor.Bi (RedeemSignature a)
