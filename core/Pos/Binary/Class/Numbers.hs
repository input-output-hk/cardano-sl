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
       , putWord8
       , getWord8
       ) where

import           Universum

import           Data.Bits              (Bits (..), FiniteBits, countLeadingZeros,
                                         finiteBitSize)
import qualified Data.ByteString        as BS
import           Data.Store             (Peek, Poke, Size (..))
import qualified Data.Store.Core        as Store
import qualified Data.Store.Internal    as Store
import           GHC.TypeLits           (ErrorMessage (..), TypeError)

import           Pos.Binary.Class.Core  (Bi (..), label, limitGet)
import           Pos.Binary.Class.Store (convertSize)

-- CSL-1122 make sure it's indeed serialized as one byte
instance Bi Word8 where
    size = Store.size
    put = Store.poke
    get = Store.peek

-- CSL-1122 these should more properly be with 'getBytes' and 'putBytes'
getWord8 :: Peek Word8
getWord8 = get @Word8

putWord8 :: Word8 -> Poke ()
putWord8 = put @Word8

----------------------------------------------------------------------------
-- Variable-sized numbers
----------------------------------------------------------------------------

-- Copied from Edward Kmett's 'bytes' library (licensed under BSD3)
putUnsignedVarInt :: (Integral a, Bits a) => a -> Poke ()
putUnsignedVarInt n
    | n < 0x80 = putWord8 $ fromIntegral n
    | otherwise = do
          putWord8 $ setBit (fromIntegral n) 7
          putUnsignedVarInt $ shiftR n 7
{-# INLINE putUnsignedVarInt #-}

-- CSL-1122: there should be tests for this.
getUnsignedVarIntSize :: (Integral a, Bits a, FiniteBits a) => a -> Int
getUnsignedVarIntSize 0 = 1
getUnsignedVarIntSize n = (logBase2 n `div` 7) + 1
  where
    logBase2 x = finiteBitSize x - 1 - countLeadingZeros x
{-# INLINE getUnsignedVarIntSize #-}

getUnsignedVarInt' :: (Integral a, Bits a, FiniteBits a) => Peek a
getUnsignedVarInt' = do
    (bytes, i) <- getWord8 >>= go
    let iBytes = Store.unsafeEncodeWith (putUnsignedVarInt i) (getUnsignedVarIntSize i)
    if BS.pack bytes /= iBytes
       then fail $ "Ambigious varInt bytes: " ++ show bytes
       else return i
  where
    go n | testBit n 7 = do
             (bs, m) <- getWord8 >>= go
             return (n:bs, shiftL m 7 .|. clearBit (fromIntegral n) 7)
         | otherwise = return ([n], fromIntegral n)
{-# INLINE getUnsignedVarInt' #-}

putSignedVarInt :: (ZZEncode a b, Integral b, Bits b) => a -> Poke ()
putSignedVarInt x = putUnsignedVarInt (zzEncode x)
{-# INLINE putSignedVarInt #-}

getSignedVarInt' :: (ZZEncode a b, Integral b, Bits b, FiniteBits b) => Peek a
getSignedVarInt' = zzDecode <$> getUnsignedVarInt'
{-# INLINE getSignedVarInt' #-}

getSignedVarIntSize :: (ZZEncode a b, Integral b, Bits b, FiniteBits b) => a -> Int
getSignedVarIntSize = getUnsignedVarIntSize . zzEncode
{-# INLINE getSignedVarIntSize #-}

getTinyVarIntSize :: Word16 -> Int
getTinyVarIntSize n
    | n <= 0b1111111 = 1
    | n <= 0b11111111111111 = 2
    | otherwise =
          error "putTinyVarIntSize: the number is bigger than 2^14-1"

putTinyVarInt :: Word16 -> Poke ()
putTinyVarInt n
    | n <= 0b1111111 =
          putWord8 (fromIntegral n)
    | n <= 0b11111111111111 =
          putWord8 (setBit (fromIntegral n) 7) *>
          putWord8 (fromIntegral (shiftR n 7))
    | otherwise =
          error "putTinyVarInt: the number is bigger than 2^14-1"

getTinyVarInt' :: Peek Word16
getTinyVarInt' = do
    a <- getWord8
    if testBit a 7
        then do
            b <- getWord8
            if | testBit b 7 -> fail "getTinyVarInt': more than 2 bytes"
               | b == 0      -> fail "getTinyVarInt': second byte is 0"
               | otherwise   -> pure $ shiftL (fromIntegral b) 7 .|.
                                       clearBit (fromIntegral a) 7
        else pure (fromIntegral a)

-- | Turn a signed number into an unsigned one, and back.
--
-- >>> map zzEncode [-3, 3]
-- [5, 6]
-- >>> map zzDecode [5, 6]
-- [-3, 3]
class ZZEncode a b | a -> b, b -> a where
    zzEncode :: a -> b
    zzDecode :: b -> a

-- Copied from the 'protobuf' library (licensed under BSD3)
instance ZZEncode Int32 Word32 where
    zzEncode x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 31)
    {-# INLINE zzEncode #-}
    zzDecode x = fromIntegral (x `shiftR` 1) `xor`
                     negate (fromIntegral (x .&. 1))
    {-# INLINE zzDecode #-}

instance ZZEncode Int64 Word64 where
    zzEncode x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 63)
    {-# INLINE zzEncode #-}
    zzDecode x = fromIntegral (x `shiftR` 1) `xor`
                     negate (fromIntegral (x .&. 1))
    {-# INLINE zzDecode #-}

instance ZZEncode Int Word where
#if (WORD_SIZE_IN_BITS == 32)
    zzEncode x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 31)
#elif (WORD_SIZE_IN_BITS == 64)
    zzEncode x = fromIntegral ((x `shiftL` 1) `xor` x `shiftR` 63)
    {-# INLINE zzEncode #-}
#else
# error Unsupported platform
#endif
    zzDecode x = fromIntegral (x `shiftR` 1) `xor`
                     negate (fromIntegral (x .&. 1))
    {-# INLINE zzDecode #-}

----------------------------------------------------------------------------
-- Int/Word encoding
----------------------------------------------------------------------------

-- | A newtype wrapper for non-negative varints. During serialization its
-- contents will be encoded as a variable-sized integer.
--
-- Despite its name, e.g. @UnsignedVarInt (-50 :: Int)@ will be serialized
-- and deserialized correctly; however, 'UnsignedVarInt' is optimized for
-- non-negative numbers, and will always take maximum space (e.g. 10 bytes in
-- case of 'Int64'). Specifically, @Int@ is simply coerced into its @Word@
-- representation before being serialized.
newtype UnsignedVarInt a = UnsignedVarInt {getUnsignedVarInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData, Functor)

-- | A newtype wrapper for varints. Uses zig-zag encoding to serialize
-- negative integers – e.g. @-3@ is turned into 5, @-4@ is turned into 7,
-- etc; thus it's fair but less optimal for positive integers.
newtype SignedVarInt a = SignedVarInt {getSignedVarInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData, Functor)

-- | A newtype wrapper for non-negative integers less than @2^14@. Use it if
-- you want to be extra careful. It is guaranteed to take either 1 or 2 bytes
-- (the standard decoder for varints can consume an unlimited amount of
-- bytes).
newtype TinyVarInt = TinyVarInt {getTinyVarInt :: Word16}
    deriving (Eq, Ord, Show, Generic, NFData)

-- | A newtype wrapper for signifying that an integer should be serialized
-- using a fixed amount of bytes.
newtype FixedSizeInt a = FixedSizeInt {getFixedSizeInt :: a}
    deriving (Eq, Ord, Show, Generic, NFData, Functor)

instance TypeError
    ('Text "Do not encode 'Int' directly. Instead, use one of newtype wrappers:" ':$$:
     'Text "  'FixedSizeInt': always uses 8 bytes" ':$$:
     'Text "  'SignedVarInt': uses 1–10 bytes (1 byte for −64..63)" ':$$:
     'Text "  'UnsignedVarInt': uses 1–10 bytes (1 byte for 0..127);" ':$$:
     'Text "                    more efficient for non-negative numbers," ':$$:
     'Text "                    but takes 10 bytes for negative numbers")
  => Bi Int
  where
    get = error "get@Int"
    put = error "put@Int"
    size = error "size@Int"

instance TypeError
    ('Text "Do not encode 'Word' directly. Instead, use one of newtype wrappers:" ':$$:
     'Text "  'FixedSizeInt': always uses 8 bytes" ':$$:
     'Text "  'UnsignedVarInt': uses 1–10 bytes (1 byte for 0..127)")
  => Bi Word
  where
    get = error "get@Word"
    put = error "put@Word"
    size = error "size@Word"

-- Int

instance Bi (UnsignedVarInt Int) where
    put (UnsignedVarInt a) = putUnsignedVarInt (fromIntegral a :: Word)
    {-# INLINE put #-}
    get = label "UnsignedVarInt Int" $
        UnsignedVarInt . (fromIntegral :: Word -> Int) <$>
        limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ \(UnsignedVarInt a) ->
              getUnsignedVarIntSize (fromIntegral a :: Word)
    {-# INLINE size #-}

instance Bi (SignedVarInt Int) where
    put (SignedVarInt a) = putSignedVarInt a
    {-# INLINE put #-}
    get = label "SignedVarInt Int" $
        SignedVarInt <$> limitGet 15 getSignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ getSignedVarIntSize . getSignedVarInt
    {-# INLINE size #-}


-- Is this instance valid at all? Is it int32 or int64 after all?
instance Bi (FixedSizeInt Int) where
    put (FixedSizeInt a) = Store.poke a -- CSL-1122 fix endianess
    {-# INLINE put #-}
    get = label "FixedSizeInt Int" $
        FixedSizeInt <$> Store.peek -- CSL-1122 fix endianess
    {-# INLINE get #-}
    size = convertSize getFixedSizeInt Store.size
    {-# INLINE size #-}

-- Int64

instance Bi (UnsignedVarInt Int64) where
    put (UnsignedVarInt a) = putUnsignedVarInt (fromIntegral a :: Word64)
    {-# INLINE put #-}
    get = label "UnsignedVarInt Int64" $
        UnsignedVarInt . (fromIntegral :: Word64 -> Int64) <$>
        limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ \(UnsignedVarInt a) ->
              getUnsignedVarIntSize (fromIntegral a :: Word64)
    {-# INLINE size #-}

instance Bi (SignedVarInt Int64) where
    put (SignedVarInt a) = putSignedVarInt a
    {-# INLINE put #-}
    get = label "SignedVarInt Int64" $
        SignedVarInt <$> limitGet 15 getSignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ getSignedVarIntSize . getSignedVarInt
    {-# INLINE size #-}

instance Bi (FixedSizeInt Int64) where
    put (FixedSizeInt a) = Store.poke a -- CSL-1122 fix endianess
    {-# INLINE put #-}
    get = label "FixedSizeInt Int64" $
        FixedSizeInt <$> Store.peek -- CSL-1122 fix endianess
    {-# INLINE get #-}
    size = convertSize getFixedSizeInt Store.size
    {-# INLINE size #-}

-- Word

instance Bi (UnsignedVarInt Word) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ getUnsignedVarIntSize . getUnsignedVarInt
    {-# INLINE size #-}

-- Is this instance valid at all? Is it word32 or word64 after all?
instance Bi (FixedSizeInt Word) where
    put (FixedSizeInt a) = Store.poke a -- CSL-1122 fix endianess
    {-# INLINE put #-}
    get = label "FixedSizeInt Word" $
        FixedSizeInt <$> Store.peek -- CSL-1122 fix endianess
    {-# INLINE get #-}
    size = convertSize getFixedSizeInt Store.size
    {-# INLINE size #-}

-- Word16

instance Bi (UnsignedVarInt Word16) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word16" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ getUnsignedVarIntSize . getUnsignedVarInt
    {-# INLINE size #-}

-- Word32

instance Bi (UnsignedVarInt Word32) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word32" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ getUnsignedVarIntSize . getUnsignedVarInt
    {-# INLINE size #-}

-- Word64

instance Bi (UnsignedVarInt Word64) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word64" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}
    size = VarSize $ getUnsignedVarIntSize . getUnsignedVarInt
    {-# INLINE size #-}

-- TinyVarInt

instance Bi TinyVarInt where
    put (TinyVarInt a) = putTinyVarInt a
    {-# INLINE put #-}
    -- Doesn't need 'limitGet' because 'TinyVarInt' is already limited to two
    -- bytes
    get = label "TinyVarInt" $
        TinyVarInt <$> getTinyVarInt'
    {-# INLINE get #-}
    size = VarSize $ getTinyVarIntSize . getTinyVarInt
    {-# INLINE size #-}
