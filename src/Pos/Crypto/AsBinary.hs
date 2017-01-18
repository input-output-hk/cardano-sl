{-# LANGUAGE CPP                   #-}

-- | AsBinary wrappers for Pos.Crypto.SecretSharing types.

module Pos.Crypto.AsBinary () where

import qualified Data.ByteString     as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (decodeFull, encode, encodeStrict)
import           Pos.Binary.Crypto ()
import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssPublicKey (..))
import           Pos.Util                 (AsBinary (..), AsBinaryClass (..))


----------------------------------------------------------------------------
-- AsBinary type wrappers
--
-- wrappers over byte string to make it possible to transmit
-- crypto data types over network without high costs on serialization/hashing
----------------------------------------------------------------------------

checkLen :: Text -> Text -> Int -> ByteString -> ByteString
checkLen action name len bs =
    maybe bs panic $ checkLenImpl action name len $ BS.length bs

checkLenL :: Text -> Text -> Int64 -> LByteString -> LByteString
checkLenL action name len bs =
    maybe bs panic $ checkLenImpl action name len $ LBS.length bs

checkLenImpl :: Integral a => Text -> Text -> a -> a -> Maybe Text
checkLenImpl action name expectedLen len
    | expectedLen == len = Nothing
    | otherwise =
        Just $
        sformat
            (stext % " " %stext % " failed: length of bytestring is " %int %
             " instead of " %int)
            action
            name
            len
            expectedLen

#define Ser(B, Bytes, Name) \
  instance AsBinaryClass B where {\
    asBinary = AsBinary . checkLen "asBinary" Name Bytes . encodeStrict ;\
    fromBinary = decodeFull . checkLenL "fromBinary" Name Bytes . encode }; \

Ser(VssPublicKey, 33, "VssPublicKey")
Ser(Secret, 33, "Secret")
Ser(Share, 101, "Share") --4+33+64
Ser(EncShare, 101, "EncShare")
Ser(SecretProof, 64, "SecretProof")

instance Buildable (AsBinary Secret) where
    build _ = "secret ¯\\_(ツ)_/¯"

instance Buildable (AsBinary Share) where
    build _ = "share ¯\\_(ツ)_/¯"

instance Buildable (AsBinary EncShare) where
    build _ = "encrypted share ¯\\_(ツ)_/¯"

instance Buildable (AsBinary VssPublicKey) where
    build = bprint ("vsspub:"%shortHashF) . hash

instance AsBinaryClass SecretSharingExtra where
    asBinary = AsBinary . encodeStrict
    fromBinary = decodeFull . LBS.fromStrict . getAsBinary
