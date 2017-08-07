{-# LANGUAGE BinaryLiterals #-}
{-# LANGUAGE CPP            #-}
{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE DeriveFunctor  #-}
{-# LANGUAGE TypeOperators  #-}

#include "MachDeps.h"

-- | Primitives and convenient wrappers for serialisation/deserialisation of numbers.


module Pos.Binary.Class.Numbers
       (
        -- * Different sizes for ints
         UnsignedVarInt(..)
       , SignedVarInt(..)
       , TinyVarInt(..)
       , FixedSizeInt(..)
       ) where

import           Universum

import           Pos.Binary.Class.Core (Bi (..))

----------------------------------------------------------------------------
-- Variable-sized numbers
----------------------------------------------------------------------------

class    NonNegative a
instance NonNegative Word
instance NonNegative Word8
instance NonNegative Word16
instance NonNegative Word32
instance NonNegative Word64

----------------------------------------------------------------------------
-- Int/Word encoding
----------------------------------------------------------------------------

-- | A newtype wrapper optimized for non-negative varints. During
-- serialization its contents will be encoded as a variable-sized integer.
--
-- Despite its name, 'UnsignedVarInt' correctly works with negative numbers!
-- It will simply always take maximum space (e.g. 10 bytes in case of
-- 'Int64') for negative numbers (because @Int@ is simply coerced into its
-- @Word@ representation before being serialized).
newtype UnsignedVarInt a = UnsignedVarInt {getUnsignedVarInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData, Functor, Enum, Num, Integral, Real)

-- | A newtype wrapper for varints. Uses zig-zag encoding to serialize
-- negative integers – e.g. @-3@ is turned into 5, @-4@ is turned into 7,
-- etc; thus it's fair but less optimal for positive integers.
newtype SignedVarInt a = SignedVarInt {getSignedVarInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData, Functor, Enum, Num, Integral, Real)

-- | A newtype wrapper for non-negative integers less than @2^14@. Use it if
-- you want to be extra careful. It is guaranteed to take either 1 or 2 bytes
-- (the standard decoder for varints can consume an unlimited amount of
-- bytes).
newtype TinyVarInt = TinyVarInt {getTinyVarInt :: Word16}
    deriving (Eq, Ord, Show, Generic, NFData)

-- | A newtype wrapper for signifying that an integer should be serialized
-- using a fixed amount of bytes.
newtype FixedSizeInt a = FixedSizeInt {getFixedSizeInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData, Functor, Enum, Num, Integral, Real)

-- Int

instance Bi (UnsignedVarInt Int) where
  encode = encode . getUnsignedVarInt
  decode = UnsignedVarInt <$> decode

instance Bi (SignedVarInt Int) where
  encode = encode . getSignedVarInt
  decode = SignedVarInt <$> decode

-- Int64

instance Bi (FixedSizeInt Int) where
  encode = encode . getFixedSizeInt
  decode = FixedSizeInt <$> decode

instance Bi (UnsignedVarInt Int64) where
  encode = encode . getUnsignedVarInt
  decode = UnsignedVarInt <$> decode

instance Bi (SignedVarInt Int64) where
  encode = encode . getSignedVarInt
  decode = SignedVarInt <$> decode

instance Bi (FixedSizeInt Int64) where
  encode = encode . getFixedSizeInt
  decode = FixedSizeInt <$> decode

-- Word

instance Bi (UnsignedVarInt Word) where
  encode = encode . getUnsignedVarInt
  decode = UnsignedVarInt <$> decode

instance Bi (FixedSizeInt Word) where
  encode = encode . getFixedSizeInt
  decode = FixedSizeInt <$> decode

-- Word16

instance Bi (UnsignedVarInt Word16) where
  encode = encode . getUnsignedVarInt
  decode = UnsignedVarInt <$> decode

-- Word32

instance Bi (UnsignedVarInt Word32) where
  encode = encode . getUnsignedVarInt
  decode = UnsignedVarInt <$> decode

-- Word64

instance Bi (UnsignedVarInt Word64) where
  encode = encode . getUnsignedVarInt
  decode = UnsignedVarInt <$> decode

-- TinyVarInt
-- The new CBOR instance is going to automatically offload on the `encodeWord16` from the CBOR library.
-- The reasoning behind this is that the old implementation was taking 1 or 2 bytes according to the size,
-- but due to the fact we need flat-term encoding for CBOR instances, we need (in case the TinyVarInt occupies
-- two bytes) to store this as a list, so the space-saving would be lost anyway due to the list size, even
-- in the case the `Int` occupies only 1 byte. Therefore, it's simpler to always encode this as a `Word16`
-- directly.
instance Bi TinyVarInt where
  encode = encode . getTinyVarInt
  decode = TinyVarInt <$> decode
