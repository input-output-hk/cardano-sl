{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE ViewPatterns        #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Universum

import qualified Cardano.Crypto.Wallet      as CC
import qualified Crypto.ECC.Edwards25519    as Ed25519
import           Crypto.Hash                (digestFromByteString, hashDigestSize)
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

import           Pos.Binary.Class           (AsBinary (..), Bi (..), Size (..),
                                             StaticSize (..), getByteString, getCopyBi,
                                             label, putByteString, putCopyBi)
import qualified Pos.Binary.Class           as Bi
import           Pos.Crypto.Hashing         (AbstractHash (..), Hash, HashAlgorithm,
                                             WithHash (..), hashDigestSize',
                                             reifyHashDigestSize, withHash)
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

instance Bi a => SafeCopy (WithHash a) where
    putCopy = putCopyBi
    getCopy = getCopyBi "WithHash"

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashAlgorithm algo => Bi (AbstractHash algo a) where
    size = ConstSize (hashDigestSize' @algo)
    put (AbstractHash digest) =
        reifyHashDigestSize @algo (\(Proxy :: Proxy n) ->
            let bs = ByteArray.convert digest :: BS.ByteString
            in put (StaticSize @n bs))
    get =
        reifyHashDigestSize @algo (\(Proxy :: Proxy n) -> do
            sbs <- get
            let bs = unStaticSize @n sbs :: BS.ByteString
            case digestFromByteString bs of
                Nothing -> error "AbstractHash.peek: impossible"
                Just x  -> pure (AbstractHash x))

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

-- [CSL-1122] TODO: move elsewhere?
constantSizedBinaryToStoreGet :: Binary.Binary a => Int -> Store.Peek a
constantSizedBinaryToStoreGet bytes = do
    x <- getByteString bytes
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
    put = putByteString . BSL.toStrict . Binary.encode ;\
    get = label "T" $ constantSizedBinaryToStoreGet BYTES };\
  deriving instance Bi PT ;\

BiPvss (Pvss.PublicKey, VssPublicKey, 33)
BiPvss (Pvss.KeyPair, VssKeyPair, 66)        -- 33+33
BiPvss (Pvss.Secret, Secret, 33)
BiPvss (Pvss.DecryptedShare, Share, 101)     -- 4+33+64
BiPvss (Pvss.EncryptedShare, EncShare, 101)
BiPvss (Pvss.Proof, SecretProof, 64)

instance Store.Store Pvss.ExtraGen where
    size = ConstSize 33
    poke = putByteString . BSL.toStrict . Binary.encode
    peek = label "Pvss.ExtraGen" $ constantSizedBinaryToStoreGet 33
instance Store.Store Pvss.Commitment where
    size = ConstSize 33
    poke = putByteString . BSL.toStrict . Binary.encode
    peek = label "Pvss.Commitment" $ constantSizedBinaryToStoreGet 33

Store.makeStore ''SecretSharingExtra
instance Bi SecretSharingExtra where
    put = Store.poke; get = Store.peek; size = Store.size

deriving instance Bi (AsBinary SecretSharingExtra)

----------------------------------------------------------------------------
-- SecretSharing AsBinary
----------------------------------------------------------------------------

#define BiMacro(B, BYTES) \
  instance Bi (AsBinary B) where {\
    size = ConstSize BYTES ;\
    put (AsBinary bs) = putByteString bs ;\
    get = label "B (BYTES bytes)" $ AsBinary <$> getByteString BYTES}; \

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
    size = ConstSize publicKeyLength
    put (Ed25519.unPointCompressed -> k) = do
        putAssertLength "PointCompressed" publicKeyLength k
        putByteString k
    get = label "Ed25519.PointCompressed" $
        Ed25519.pointCompressed <$> getByteString publicKeyLength

instance Bi Ed25519.Scalar where
    size = ConstSize secretKeyLength
    put (Ed25519.unScalar -> k) = do
        putAssertLength "Scalar" secretKeyLength k
        putByteString k
    get = label "Ed25519.Scalar" $
        Ed25519.scalar <$> getByteString secretKeyLength

instance Bi Ed25519.Signature where
    size = ConstSize signatureLength
    put (Ed25519.Signature s) = do
        putAssertLength "Signature" signatureLength s
        putByteString s
    get = label "Ed25519.Signature" $
        Ed25519.Signature <$> getByteString signatureLength

instance Bi CC.ChainCode where
    size = ConstSize chainCodeLength
    put (CC.ChainCode c) = do
        putAssertLength "ChainCode" chainCodeLength c
        putByteString c
    get = label "CC.ChainCode" $
        CC.ChainCode <$> getByteString chainCodeLength

instance Bi CC.XPub where
    size = ConstSize (publicKeyLength + chainCodeLength)
    put (CC.unXPub -> kc) = do
        putAssertLength "XPub" (publicKeyLength + chainCodeLength) kc
        putByteString kc
    get = label "CC.XPub" $
        getByteString (publicKeyLength + chainCodeLength) >>=
        either fail pure . CC.xpub

instance Bi CC.XPrv where
    size = ConstSize encryptedKeyLength
    put (CC.unXPrv -> kc) = do
        putAssertLength "XPrv" encryptedKeyLength kc
        putByteString kc
    get = label "CC.XPrv" $
        getByteString encryptedKeyLength >>=
        either fail pure . CC.xprv

instance Bi CC.XSignature where
    size = ConstSize signatureLength
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
    size = Bi.combineSize (eskPayload, eskHash)
    put (EncryptedSecretKey sk pph) = put sk >> put pph
    get = label "EncryptedSecretKey" $ liftM2 EncryptedSecretKey get get

instance Bi a => Bi (Signed a) where
    size = Bi.combineSize (signedValue, signedSig)
    put (Signed v s) = put (v,s)
    get = label "Signed" $ Signed <$> get <*> get

deriving instance Bi (ProxyCert w)

instance (Bi w) => Bi (ProxySecretKey w) where
    put (ProxySecretKey w iPk dPk cert) = put w >> put iPk >> put dPk >> put cert
    get = label "ProxySecretKey" $ liftM4 ProxySecretKey get get get get

instance (Bi w) => Bi (ProxySignature w a) where
    put ProxySignature{..} = do
        put pdPsk
        put pdSig
    get = label "ProxySignature" $ liftM2 ProxySignature get get

instance Bi PassPhrase where
    size = ConstSize passphraseLength
    put pp = do
        -- currently passphrase may be 32-byte long, or empty (for
        -- unencrypted keys). The empty passphrase is serialized as 32
        -- zeroes.
        let bs = ByteArray.convert pp
            bl = BS.length bs
        if | bl == 0 -> putByteString (BS.replicate passphraseLength 0)
           | bl == passphraseLength -> putByteString bs
           | otherwise -> error $ sformat
                 ("put@PassPhrase: expected length 0 or "%int%", not "%int)
                 passphraseLength bl
    get = let norm x | x == BS.replicate passphraseLength 0 = mempty
                     | otherwise                            = x
          in  label "PassPhrase" $
              ByteArray.convert . norm <$>
              getByteString passphraseLength

-------------------------------------------------------------------------------
-- Hierarchical derivation
-------------------------------------------------------------------------------

-- CSL-1122 uncomment
instance Bi HDAddressPayload where
--    put = Store.poke
--    {-# INLINE put #-}
--    get = Store.peek
--    {-# INLINE get #-}
--    size = Store.size
--    {-# INLINE size #-}

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
