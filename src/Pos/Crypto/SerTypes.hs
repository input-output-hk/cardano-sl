{-# LANGUAGE CPP                   #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}

{-| Serialized types for implementation of VSS (wrapping over pvss).
    For more details see <https://github.com/input-output-hk/pvss-haskell>.
-}


module Pos.Crypto.SerTypes
       ( LVssPublicKey
       , LSecret
       , LShare
       , LEncShare
       , LSecretProof
       , LSecretSharingExtra
       ) where

import           Data.Aeson               (ToJSON (toJSON))
import           Data.Binary              (Binary (..), encode)
import           Data.Binary.Get          (getByteString)
import           Data.Binary.Put          (putByteString)
import qualified Data.ByteString.Lazy     as LBS
import           Data.Hashable            (Hashable)
import qualified Data.Hashable            as Hashable
import           Data.SafeCopy            (base, deriveSafeCopySimple)
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))
import           Serokell.Util.Binary     as Binary (decodeFull)
import           Universum                hiding (putByteString)

import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (EncShare, Secret, SecretProof,
                                           SecretSharingExtra, Share, VssPublicKey)
import           Pos.Util                 (Serialized (..))


decodeErr :: Binary b => Text -> LBS.ByteString -> b
decodeErr s = either (panic . (\e -> "Decoding " <> s <> " failed: " <> show e)) identity . decodeFull

----------------------------------------------------------------------------
-- Lightweight type wrappers
--
-- wrappers over byte string to make it possible to transmit
-- crypto data types over network without high costs on serialization/hashing
----------------------------------------------------------------------------

#define Ser(B, A, Bytes, Name) \
  newtype A = A ByteString \
    deriving (Show, Eq) ;\
  instance Binary A where {\
    put (A bs) = putByteString bs ;\
    get = A <$> getByteString Bytes}; \
  instance Serialized B A where {\
    serialize = decodeErr Name . checkLen "serialize" Name Bytes . encode ;\
    deserialize = decodeFull . checkLen "deserialize" Name Bytes . encode }; \
  deriveSafeCopySimple 0 'base ''A

checkLen :: Text -> Text -> Int -> LBS.ByteString -> LBS.ByteString
checkLen action name len bs =
    if LBS.length bs == fromIntegral len
        then bs
        else panic $ sformat
                (stext % " " % stext % " failed: length of bytestring is " % int % " instead of " % int)
                action name (LBS.length bs) len



Ser(VssPublicKey, LVssPublicKey, 33, "LVssPublicKey")
Ser(Secret, LSecret, 33, "LSecret")
Ser(Share, LShare, 101, "LShare") --4+33+64
Ser(EncShare, LEncShare, 101, "LEncShare")
Ser(SecretProof, LSecretProof, 64, "LSecretProof")

instance Hashable LVssPublicKey where
    hashWithSalt s = Hashable.hashWithSalt s . encode

instance Buildable LSecret where
    build _ = "secret ¯\\_(ツ)_/¯"

instance ToJSON LSecret where
    toJSON = notImplemented

instance Buildable LShare where
    build _ = "share ¯\\_(ツ)_/¯"

instance Buildable LEncShare where
    build _ = "encrypted share ¯\\_(ツ)_/¯"

instance Buildable LVssPublicKey where
    build = bprint ("vsspub:"%shortHashF) . hash


newtype LSecretSharingExtra = LSecretSharingExtra LBS.ByteString
    deriving (Show, Eq, Binary)

instance Serialized SecretSharingExtra LSecretSharingExtra where
  serialize = LSecretSharingExtra . encode
  deserialize (LSecretSharingExtra x) = decodeFull x

deriveSafeCopySimple 0 'base ''LSecretSharingExtra
