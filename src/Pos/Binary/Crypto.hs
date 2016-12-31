{-# LANGUAGE CPP                   #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

-- | Serializable instances for Pos.Crypto.*

module Pos.Binary.Crypto () where

import           Control.Monad.Fail       (fail)
import           Crypto.Hash              (digestFromByteString, hashDigestSize)
import qualified Crypto.PVSS              as Pvss
import qualified Crypto.Sign.Ed25519      as Ed25519
import qualified Data.Binary              as Binary
import           Data.Binary.Get          (getByteString)
import           Data.Binary.Put          (putByteString)
import qualified Data.ByteArray           as ByteArray
import qualified Data.ByteString          as BS
import           Data.SafeCopy            (SafeCopy (..))
import           Formatting               (int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (Bi (..))
import           Pos.Crypto.Hashing       (AbstractHash (..), Hash, HashAlgorithm,
                                           WithHash (..), withHash)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssKeyPair (..), VssPublicKey (..))
import           Pos.Crypto.Signing       (ProxyCert (..), ProxySecretKey (..),
                                           ProxySignature (..), PublicKey (..),
                                           SecretKey (..), Signature (..), Signed (..))
import           Pos.Util                 (AsBinary (..), getCopyBinary, putCopyBinary)

instance Bi a => Bi (WithHash a) where
    put = put . whData
    get = withHash <$> get

instance Bi a => SafeCopy (WithHash a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "WithHash"

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

instance HashAlgorithm algo => Bi (AbstractHash algo a) where
    {-# SPECIALIZE instance Bi (Hash a) #-}
    get = do
        bs <- getByteString $ hashDigestSize @algo $
              panic "Pos.Crypto.Hashing.get: HashAlgorithm value is evaluated!"
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
    get = Binary.get }; \
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
    get = AsBinary <$> getByteString Bytes}; \

BiMacro(VssPublicKey, 33)
BiMacro(Secret, 33)
BiMacro(Share, 101) --4+33+64
BiMacro(EncShare, 101)
BiMacro(SecretProof, 64)

----------------------------------------------------------------------------
-- Signing
----------------------------------------------------------------------------

secretKeyLength, publicKeyLength, signatureLength :: Int
secretKeyLength = 64
publicKeyLength = 32
signatureLength = 64

putAssertLength :: Monad m => Text -> Int -> ByteString -> m ()
putAssertLength typeName expectedLength bs =
    when (BS.length bs /= expectedLength) $ panic $
        sformat ("put@"%stext%": expected length "%int%", not "%int)
                typeName expectedLength (BS.length bs)

instance Bi Ed25519.PublicKey where
    put (Ed25519.PublicKey k) = do
        putAssertLength "PublicKey" publicKeyLength k
        putByteString k
    get = Ed25519.PublicKey <$> getByteString publicKeyLength

instance Bi Ed25519.SecretKey where
    put (Ed25519.SecretKey k) = do
        putAssertLength "SecretKey" secretKeyLength k
        putByteString k
    get = Ed25519.SecretKey <$> getByteString secretKeyLength

instance Bi Ed25519.Signature where
    put (Ed25519.Signature s) = do
        putAssertLength "Signature" signatureLength s
        putByteString s
    get = Ed25519.Signature <$> getByteString signatureLength

deriving instance Bi (Signature a)
deriving instance Bi PublicKey
deriving instance Bi SecretKey

instance Bi a => Bi (Signed a) where
    put (Signed v s) = put (v,s)
    get = Signed <$> get <*> get

deriving instance Bi (ProxyCert w)

instance (Bi w) => Bi (ProxySecretKey w) where
    put (ProxySecretKey w iPk cert) = put w >> put iPk >> put cert
    get = liftM3 ProxySecretKey get get get

instance (Bi w) => Bi (ProxySignature w a) where
    put ProxySignature{..} = do
        put pdOmega
        put pdDelegatePk
        put pdCert
        put pdSig
    get = liftM4 ProxySignature get get get get
