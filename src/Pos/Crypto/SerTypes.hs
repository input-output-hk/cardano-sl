{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE UndecidableInstances  #-}

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

import qualified Data.ByteString.Lazy     as LBS
import           Data.Hashable            (Hashable)
import qualified Data.Hashable            as Hashable
import           Data.SafeCopy            (base, deriveSafeCopySimple)
import           Data.Text.Buildable      (Buildable)
import qualified Data.Text.Buildable      as Buildable
import           Formatting               (bprint, int, sformat, stext, (%))
import           Universum                hiding (putByteString)

import           Pos.Binary.Class         (Bi, decode, decodeFull, encode)
import           Pos.Crypto.Hashing       (hash, shortHashF)
import           Pos.Crypto.SecretSharing (EncShare, Secret, SecretProof,
                                           SecretSharingExtra, Share, VssPublicKey)
import           Pos.Util                 (Serialized (..))


decodeErr :: Bi b => Text -> LBS.ByteString -> b
decodeErr s =
    either (panic . (\e -> "Decoding " <> s <> " failed: " <> show e)) identity .
    decodeFull

----------------------------------------------------------------------------
-- Lightweight type wrappers
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

newtype LVssPublicKey =
    LVssPublicKey ByteString
    deriving (Show, Eq)

-- TODO Move all this generated trash somewhere else
-- -- import           Data.Binary.Get          (getByteString)
-- -- import           Data.Binary.Put          (putByteString)
--
-- instance Binary LVssPublicKey where
--     put (LVssPublicKey bs) = putByteString bs
--     get = LVssPublicKey <$> getByteString 33

--instance Serialized VssPublicKey LVssPublicKey where
--    serialize =
--        decodeErr "LVssPublicKey" . checkLen "serialize" "LVssPublicKey" 33 . encode
--    deserialize = decodeFull . checkLen "deserialize" "LVssPublicKey" 33 . encode
--
--deriveSafeCopySimple 0 'base ''LVssPublicKey
newtype LSecret =
    LSecret ByteString
    deriving (Show, Eq)

--instance Binary LSecret where
--    put (LSecret bs) = putByteString bs
--    get = LSecret <$> getByteString 33

--instance Serialized Secret LSecret where
--    serialize = decodeErr "LSecret" . checkLen "serialize" "LSecret" 33 . encode
--    deserialize = decodeFull . checkLen "deserialize" "LSecret" 33 . encode
--
--deriveSafeCopySimple 0 'base ''LSecret
newtype LShare =
    LShare ByteString
    deriving (Show, Eq)

--instance Binary LShare where
--    put (LShare bs) = putByteString bs
--    get = LShare <$> getByteString 101

--instance Serialized Share LShare where
--    serialize = decodeErr "LShare" . checkLen "serialize" "LShare" 101 . encode
--    deserialize = decodeFull . checkLen "deserialize" "LShare" 101 . encode
--
--deriveSafeCopySimple 0 'base ''LShare

--4+33+64
newtype LEncShare =
    LEncShare ByteString
    deriving (Show, Eq)

--instance Binary LEncShare where
--    put (LEncShare bs) = putByteString bs
--    get = LEncShare <$> getByteString 101

--instance Serialized EncShare LEncShare where
--    serialize = decodeErr "LEncShare" . checkLen "serialize" "LEncShare" 101 . encode
--    deserialize = decodeFull . checkLen "deserialize" "LEncShare" 101 . encode
--
--deriveSafeCopySimple 0 'base ''LEncShare
newtype LSecretProof =
    LSecretProof ByteString
    deriving (Show, Eq)

--instance Binary LSecretProof where
--    put (LSecretProof bs) = putByteString bs
--    get = LSecretProof <$> getByteString 64

--instance Serialized SecretProof LSecretProof where
--    serialize =
--        decodeErr "LSecretProof" . checkLen "serialize" "LSecretProof" 64 . encode
--    deserialize = decodeFull . checkLen "deserialize" "LSecretProof" 64 . encode
--
--deriveSafeCopySimple 0 'base ''LSecretProof

instance (Bi LVssPublicKey) => Hashable LVssPublicKey where
    hashWithSalt s = Hashable.hashWithSalt s . encode

instance Buildable LSecret where
    build _ = "secret ¯\\_(ツ)_/¯"

instance Buildable LShare where
    build _ = "share ¯\\_(ツ)_/¯"

instance Buildable LEncShare where
    build _ = "encrypted share ¯\\_(ツ)_/¯"

instance (Bi LVssPublicKey) => Buildable LVssPublicKey where
    build = bprint ("vsspub:"%shortHashF) . hash

newtype LSecretSharingExtra = LSecretSharingExtra LBS.ByteString
    deriving (Show, Eq)

--instance Serialized SecretSharingExtra LSecretSharingExtra where
--  serialize = LSecretSharingExtra . encode
--  deserialize (LSecretSharingExtra x) = decodeFull x

deriveSafeCopySimple 0 'base ''LSecretSharingExtra
