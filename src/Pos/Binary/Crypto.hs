{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TypeApplications      #-}

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
import qualified Data.ByteString.Lazy     as LBS
import           Data.Hashable            (Hashable)
import qualified Data.Hashable            as Hashable
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Data.SafeCopy            (SafeCopy (..))
import           Formatting               (bprint, int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (Bi (..), decodeFull, encode)
import           Pos.Crypto.Hashing       (AbstractHash (..), Hash, HashAlgorithm,
                                           WithHash (..), hash, shortHashF, withHash)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssKeyPair (..), VssPublicKey (..))
import           Pos.Crypto.Signing       (ProxyCert (..), ProxySecretKey (..),
                                           ProxySignature (..), PublicKey (..),
                                           SecretKey (..), Signature (..), Signed (..))
import           Pos.Util                 (AsBinary (..), AsBinaryClass (..),
                                           getCopyBinary, putCopyBinary)

instance Bi a => Bi (WithHash a) where
    put = put. whData
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

----------------------------------------------------------------------------
-- SerTypes
----------------------------------------------------------------------------

----------------------------------------------------------------------------
-- AsBinary type wrappers
--
-- wrappers over byte string to make it possible to transmit
-- crypto data types over network without high costs on serialization/hashing
----------------------------------------------------------------------------

checkLen :: Text -> Text -> Int -> LBS.ByteString -> LBS.ByteString
checkLen action name len bs =
    if LBS.length bs == fromIntegral len
        then bs
        else panic $ sformat
                (stext%" "%stext%" failed: length of bytestring is "%int%" instead of "%int)
                action name (LBS.length bs) len

-- [CSL-246]: avoid boilerplate.
#define Ser(B, Bytes, Name) \
  instance Bi (AsBinary B) where {\
    put (AsBinary bs) = putByteString bs ;\
    get = AsBinary <$> getByteString Bytes}; \
  instance AsBinaryClass B where {\
    asBinary = AsBinary . LBS.toStrict . checkLen "asBinary" Name Bytes . encode ;\
    fromBinary = decodeFull . checkLen "fromBinary" Name Bytes . encode }; \

Ser(VssPublicKey, 33, "VssPublicKey")
Ser(Secret, 33, "Secret")
Ser(Share, 101, "Share") --4+33+64
Ser(EncShare, 101, "EncShare")
Ser(SecretProof, 64, "SecretProof")

instance Hashable (AsBinary VssPublicKey) where
    hashWithSalt s = Hashable.hashWithSalt s . encode

instance Buildable (AsBinary Secret) where
    build _ = "secret ¯\\_(ツ)_/¯"

instance Buildable (AsBinary Share) where
    build _ = "share ¯\\_(ツ)_/¯"

instance Buildable (AsBinary EncShare) where
    build _ = "encrypted share ¯\\_(ツ)_/¯"

instance Buildable (AsBinary VssPublicKey) where
    build = bprint ("vsspub:"%shortHashF) . hash

deriving instance Bi (AsBinary SecretSharingExtra)

instance AsBinaryClass SecretSharingExtra where
    asBinary = AsBinary . LBS.toStrict . encode
    fromBinary = decodeFull . LBS.fromStrict . getAsBinary

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
