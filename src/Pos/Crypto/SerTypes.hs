{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving    #-}
{-# LANGUAGE TemplateHaskell       #-}

{-| Serialized types for implementation of VSS (wrapping over pvss).
    For more details see <https://github.com/input-output-hk/pvss-haskell>.
-}

module Pos.Crypto.SerTypes () where

import           Data.Binary              (Binary (..), encode)
import           Data.Binary.Get          (getByteString)
import           Data.Binary.Put          (putByteString)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Hashable            (Hashable)
import qualified Data.Hashable            as Hashable
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))
import           Serokell.Util.Binary     as Binary (decodeFull)
import           Universum                hiding (putByteString)

import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (EncShare, Secret, SecretProof,
                                           SecretSharingExtra, Share, VssPublicKey)
import           Pos.Util                 (AsBinary (..), Serialized (..))

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
                (stext % " " % stext % " failed: length of bytestring is " % int % " instead of " % int)
                action name (LBS.length bs) len

-- [CSL-246]: avoid boilerplate.
-- | This type is used to provide space-efficient serialization for the crypto
-- primitives used in the PVSS scheme. A phantom type is used here to maintain
-- type-safety since for all types `a`, `AsBinary a` is just a strict
-- bytestring inside a newtype.

#define Ser(B, Bytes, Name) \
  instance Binary (AsBinary B) where {\
    put (AsBinary bs) = putByteString bs ;\
    get = AsBinary <$> getByteString Bytes}; \
  instance Serialized B where {\
    serialize = AsBinary . LBS.toStrict . checkLen "serialize" Name Bytes . encode ;\
    deserialize = decodeFull . checkLen "deserialize" Name Bytes . encode }; \

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

deriving instance Binary (AsBinary SecretSharingExtra)

instance Serialized SecretSharingExtra where
    serialize = AsBinary . LBS.toStrict . encode
    deserialize = decodeFull . LBS.fromStrict . getAsBinary
