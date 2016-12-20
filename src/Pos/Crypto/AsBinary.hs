{-# LANGUAGE CPP                   #-}
{-# LANGUAGE FlexibleInstances     #-}

-- | Lightweight serialization for Pos.Crypto.SecretSharing

module Pos.Crypto.AsBinary () where

import           Data.Binary.Get          (getByteString)
import           Data.Binary.Put          (putByteString)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Hashable            (Hashable)
import qualified Data.Hashable            as Hashable
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (Bi (..), decodeFull, encode)
import           Pos.Binary.Crypto        ()
import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssPublicKey (..))
import           Pos.Util                 (AsBinary (..), AsBinaryClass (..))


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

instance AsBinaryClass SecretSharingExtra where
    asBinary = AsBinary . LBS.toStrict . encode
    fromBinary = decodeFull . LBS.fromStrict . getAsBinary
