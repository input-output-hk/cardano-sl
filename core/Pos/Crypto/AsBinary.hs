{-# LANGUAGE CPP #-}

-- | AsBinary wrappers for Pos.Crypto.SecretSharing types.

module Pos.Crypto.AsBinary (
      vssPublicKeyBytes
    , secretBytes
    , shareBytes
    , encShareBytes
    , secretProofBytes
    ) where

import           Universum

import qualified Data.ByteString          as BS
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))

import           Pos.Binary.Class         (AsBinary (..), AsBinaryClass (..), Bi,
                                           decodeFull, serialize')
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
    maybe bs error $ checkLenImpl action name len $ BS.length bs

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
    asBinary = AsBinary . checkLen "asBinary" Name Bytes . serialize' ;\
    fromBinary = decodeFull . checkLen "fromBinary" Name Bytes . getAsBinary }; \


vssPublicKeyBytes, secretBytes, shareBytes, encShareBytes, secretProofBytes :: Int
vssPublicKeyBytes = 35   -- 33 data + 2 of CBOR overhead
secretBytes       = 35   -- 33 data + 2 of CBOR overhead
shareBytes        = 103  --4+33+64
encShareBytes     = 103
secretProofBytes  = 66   -- 64 data + 2 of CBOR overhead

Ser(VssPublicKey, vssPublicKeyBytes, "VssPublicKey")
Ser(Secret, secretBytes, "Secret")
Ser(Share, shareBytes, "Share")
Ser(EncShare, encShareBytes, "EncShare")
Ser(SecretProof, secretProofBytes, "SecretProof")

instance Buildable (AsBinary Secret) where
    build _ = "secret \\_(o.o)_/"

instance Buildable (AsBinary Share) where
    build _ = "share \\_(*.*)_/"

instance Buildable (AsBinary EncShare) where
    build _ = "encrypted share \\_(0.0)_/"

instance Bi (AsBinary VssPublicKey) => Buildable (AsBinary VssPublicKey) where
    build = bprint ("vsspub:"%shortHashF) . hash

instance Bi SecretSharingExtra => AsBinaryClass SecretSharingExtra where
    asBinary = AsBinary . serialize'
    fromBinary = decodeFull . getAsBinary
