{-# LANGUAGE CPP                  #-}
{-# LANGUAGE UndecidableInstances #-}

-- | AsBinary wrappers for Pos.Crypto.SecretSharing types.

module Pos.Crypto.AsBinary () where

import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as LBS
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (Bi, AsBinary (..), AsBinaryClass (..),
                                           decodeFull, encode, encodeStrict)
import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (EncShare (..), Secret (..), SecretProof (..),
                                           SecretSharingExtra (..), Share (..),
                                           VssPublicKey (..))

----------------------------------------------------------------------------
-- AsBinary type wrappers
--
-- Wrappers over ByteString to allow transmitting crypto data types
-- over network without high costs on serialization/hashing
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
  instance (Bi B, Bi (AsBinary B)) => AsBinaryClass B where {\
    asBinary = AsBinary . checkLen "asBinary" Name Bytes . encodeStrict ;\
    fromBinary = decodeFull . checkLenL "fromBinary" Name Bytes . encode }; \

Ser(VssPublicKey, 33, "VssPublicKey")
Ser(Secret, 33, "Secret")
Ser(Share, 101, "Share") --4+33+64
Ser(EncShare, 101, "EncShare")
Ser(SecretProof, 64, "SecretProof")

instance Buildable (AsBinary Secret) where
    build _ = "secret \\_(o.o)_/"

instance Buildable (AsBinary Share) where
    build _ = "share \\_(*.*)_/"

instance Buildable (AsBinary EncShare) where
    build _ = "encrypted share \\_(0.0)_/"

instance Bi (AsBinary VssPublicKey) => Buildable (AsBinary VssPublicKey) where
    build = bprint ("vsspub:"%shortHashF) . hash

instance Bi SecretSharingExtra => AsBinaryClass SecretSharingExtra where
    asBinary = AsBinary . encodeStrict
    fromBinary = decodeFull . LBS.fromStrict . getAsBinary
