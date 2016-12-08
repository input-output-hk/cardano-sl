{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE UndecidableInstances #-}

{-| Serialized types for implementation of VSS (wrapping over pvss).
    For more details see <https://github.com/input-output-hk/pvss-haskell>.
-}

module Pos.Crypto.SerTypes
       ( LVssPublicKey (..)
       , LSecret (..)
       , LShare (..)
       , LEncShare (..)
       , LSecretProof (..)
       , LSecretSharingExtra (..)
       ) where

import qualified Data.ByteString.Lazy as LBS
import           Data.Hashable        (Hashable)
import qualified Data.Hashable        as Hashable
import           Data.SafeCopy        (base, deriveSafeCopySimple)
import           Data.Text.Buildable  (Buildable)
import qualified Data.Text.Buildable  as Buildable
import           Formatting           (bprint, (%))
import           Universum            hiding (putByteString)

import           Pos.Binary.Class     (Bi, encode)
import           Pos.Crypto.Hashing   (hash, shortHashF)

----------------------------------------------------------------------------
-- Lightweight type wrappers
--
-- wrappers over byte string to make it possible to transmit
-- crypto data types over network without high costs on serialization/hashing
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

newtype LVssPublicKey =
    LVssPublicKey ByteString
    deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''LVssPublicKey
newtype LSecret =
    LSecret ByteString
    deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''LSecret
newtype LShare =
    LShare ByteString
    deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''LShare
--4+33+64
newtype LEncShare =
    LEncShare ByteString
    deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''LEncShare
newtype LSecretProof =
    LSecretProof ByteString
    deriving (Show, Eq)

deriveSafeCopySimple 0 'base ''LSecretProof

instance Bi LVssPublicKey => Hashable LVssPublicKey where
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
