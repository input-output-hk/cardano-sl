{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
-- | Crypto binary/serializable/...

module Pos.Binary.Crypto where

import qualified Crypto.PVSS              as Pvss
import qualified Data.Binary              as Binary
import           Data.Binary.Get          (getByteString)
import           Data.Binary.Put          (putByteString)
import qualified Data.ByteString.Lazy     as LBS
import           Data.SafeCopy            (SafeCopy (..), base, deriveSafeCopySimple)
import           Formatting               (bprint, int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (Bi (..), Serialized (..), decode, decodeFull,
                                           encode)
import           Pos.Crypto.Hashing       (WithHash (..), withHash)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssPublicKey (..))
import           Pos.Crypto.SerTypes      (LEncShare (..), LSecret (..),
                                           LSecretProof (..), LSecretSharingExtra (..),
                                           LShare (..), LVssPublicKey (..))
import           Pos.Util                 (getCopyBinary, putCopyBinary)

instance Bi a => Bi (WithHash a) where
    put = put. whData
    get = withHash <$> get

instance Bi a => SafeCopy (WithHash a) where
    putCopy = putCopyBinary
    getCopy = getCopyBinary "WithHash"


checkLen :: Text -> Text -> Int -> LBS.ByteString -> LBS.ByteString
checkLen action name len bs =
    if LBS.length bs == fromIntegral len
        then bs
        else panic $ sformat
                (stext%" "%stext%" failed: length of bytestring is "%int%" instead of "%int)
                action name (LBS.length bs) len

----------------------------------------------------------------------------
-- Hashing
----------------------------------------------------------------------------

--instance HashAlgorithm algo => Bi (AbstractHash algo a) where
--    {-# SPECIALIZE instance Binary (Hash a) #-}
--    get = do
--        bs <- Binary.getByteString $ Hash.hashDigestSize @algo $
--              panic "Pos.Crypto.Hashing.get: HashAlgorithm value is evaluated!"
--        case digestFromByteString bs of
--            -- It's impossible because getByteString will already fail if
--            -- there weren't enough bytes available
--            Nothing -> fail "Pos.Crypto.Hashing.get: impossible"
--            Just x  -> return (AbstractHash x)
--    put (AbstractHash h) =
--        Binary.putByteString (ByteArray.convert h)

----------------------------------------------------------------------------
-- SecretSharing
----------------------------------------------------------------------------

-- TODO WRITE THEM

instance Bi Pvss.PublicKey where
    put = Binary.put
    get = Binary.get

deriving instance Bi VssPublicKey
instance Bi Secret
instance Bi Share
instance Bi EncShare
instance Bi SecretProof

----------------------------------------------------------------------------
-- SerTypes
----------------------------------------------------------------------------

-- [CSL-246]: avoid boilerplate.
{-
#define Ser(B, A, Bytes, Name) \
  newtype A = A ByteString \
    deriving (Show, Eq) ;\
  instance Binary A where {\
    put (A bs) = putByteString bs ;\
    get = A <$> getByteString Bytes}; \
  instance Serialized B A where {\
    serialize = A . LBS.toStrict . checkLen "serialize" Name Bytes . encode ;\
    deserialize = decodeFull . checkLen "deserialize" Name Bytes . encode }; \
  deriveSafeCopySimple 0 'base ''A

Ser(VssPublicKey, LVssPublicKey, 33, "LVssPublicKey")
Ser(Secret, LSecret, 33, "LSecret")
Ser(Share, LShare, 101, "LShare") --4+33+64
Ser(EncShare, LEncShare, 101, "LEncShare")
Ser(SecretProof, LSecretProof, 64, "LSecretProof")
-}

instance Bi LVssPublicKey where
    put (LVssPublicKey bs) = putByteString bs
    get = LVssPublicKey <$> getByteString 33

instance Serialized VssPublicKey LVssPublicKey where
    serialize =
        LVssPublicKey .
        LBS.toStrict . checkLen "serialize" "LVssPublicKey" 33 . encode
    deserialize = decodeFull . checkLen "deserialize" "LVssPublicKey" 33 . encode

instance Bi LSecret where
    put (LSecret bs) = putByteString bs
    get = LSecret <$> getByteString 33

instance Serialized Secret LSecret where
    serialize = LSecret . LBS.toStrict . checkLen "serialize" "LSecret" 33 . encode
    deserialize = decodeFull . checkLen "deserialize" "LSecret" 33 . encode


instance Bi LShare where
    put (LShare bs) = putByteString bs
    get = LShare <$> getByteString 101

instance Serialized Share LShare where
    serialize = LShare . LBS.toStrict . checkLen "serialize" "LShare" 101 . encode
    deserialize = decodeFull . checkLen "deserialize" "LShare" 101 . encode


instance Bi LEncShare where
    put (LEncShare bs) = putByteString bs
    get = LEncShare <$> getByteString 101

instance Serialized EncShare LEncShare where
    serialize =
        LEncShare . LBS.toStrict . checkLen "serialize" "LEncShare" 101 . encode
    deserialize = decodeFull . checkLen "deserialize" "LEncShare" 101 . encode


instance Bi LSecretProof where
    put (LSecretProof bs) = putByteString bs
    get = LSecretProof <$> getByteString 64

instance Serialized SecretProof LSecretProof where
    serialize =
        LSecretProof . LBS.toStrict . checkLen "serialize" "LSecretProof" 64 . encode
    deserialize = decodeFull . checkLen "deserialize" "LSecretProof" 64 . encode
