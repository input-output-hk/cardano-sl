{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE CPP                 #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators       #-}

#include "MachDeps.h"

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Serialization-related types

module Pos.Binary.Class
       ( Bi (..)
       , encode
       , decodeFull
       , getSize
       {-
       , encode
       , decode
       , decodeOrFail
       , decodeFull
       -}

       -- * Store re-exports
       , Size(..)
       , Peek
       , Poke
       -- * Primitives for serialization
       , getWord8
       , putWord8
       , putWord8WithSize
       , getBytes
       , putBytes
       , label

       -- * Poke with size of the poke.
       , PokeWithSize (..)
       , pokeWithSize

       -- * The 'StaticSize' wrapper
       , StaticSize(..)

       -- * The 'Raw' wrapper
       , Raw

       -- * Different sizes for ints
       , UnsignedVarInt(..)
       , SignedVarInt(..)
       , TinyVarInt(..)
       , FixedSizeInt(..)

       -- * Primitives for limiting serialization
       , isEmptyPeek
       , lookAhead
       , limitGet
       , isolate64Full
       -- * Bi to SafeCopy
       , getCopyBi
       , putCopyBi
       -- * Binary serialization
       , AsBinary (..)
       , AsBinaryClass (..)
       , fromBinaryM

       -- * Serialization with length
       , putWithLength
       , getWithLength
       , getWithLengthLimited
       , putSmallWithLength
       , getSmallWithLength

       {-
       -- * Other binary utils
       , getRemainingByteString
       -}
       , getAsciiString1b
       , putAsciiString1b
       , sizeAsciiString1b
       , biSize

       , convertSize
       , combineSize
       , sizeOf
       , sizeAddField
       ) where

import           Universum

{-
import           Data.Binary                 (Get, Put)
import qualified Data.Binary                 as Binary
import           Data.Binary.Get             (ByteOffset, getBytes,
                                              getLazyByteString,
                                              getRemainingLazyByteString, getWord8, label,
                                              runGet, runGetOrFail)
import           Data.Binary.Get.Internal    (Decoder (..), runCont)
import           Data.Binary.Put             (PutM, putByteString, putCharUtf8,
                                              putLazyByteString, putWord8, runPut,
                                              runPutM)
import           Data.Hashable               (Hashable (..))
import           Unsafe.Coerce               (unsafeCoerce)
-}

import           Control.Lens                (_Left)
import           Data.Bits                   (Bits (..), FiniteBits, countLeadingZeros,
                                              finiteBitSize)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Internal    as BS
import           Data.Char                   (isAscii)
import           Data.Functor.Contravariant  (contramap)
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import           Data.Reflection             (reifyNat)
import           Data.SafeCopy               (Contained, SafeCopy (..), contain, safeGet,
                                              safePut)
import qualified Data.Serialize              as Cereal (Get, Put)
import qualified Data.Set                    as S
import           Data.Store                  (Size (..))
import           Data.Store.Core             (Peek (..), PeekResult (..), Poke (..))
import qualified Data.Store.Core             as Store
import           Data.Store.Internal         (PeekException (..), StaticSize (..))
import qualified Data.Store.Internal         as Store
import           Data.Tagged                 (Tagged (..))
import qualified Data.Text                   as T
import           Data.Time.Units             (Microsecond, Millisecond)
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Word                   (Word32)
import           Foreign.Ptr                 (minusPtr, plusPtr)
import           Formatting                  (formatToString, int, (%))
import           GHC.TypeLits                (ErrorMessage (..), TypeError)
import           Serokell.Data.Memory.Units  (Byte, fromBytes, toBytes)
import           System.IO.Unsafe            (unsafePerformIO)

----------------------------------------------------------------------------
-- Bi typeclass
----------------------------------------------------------------------------

-- | Simplified definition of serializable object
-- Data.Binary.Class-alike.
--
-- Write @instance Bi SomeType where@ without any method definitions if you
-- want to use the 'Binary' instance for your type.
class Bi t where
    size :: Size t
    put :: t -> Poke ()
    get :: Peek t

--instance Serializable t => B.Binary t where
--    get = get
--    put = put

-- | Encode a value to a strict bytestring
encode :: Bi a => a -> ByteString
encode x = Store.unsafeEncodeWith (put x) (getSize x)
{-# INLINE encode #-}

getSize :: Bi a => a -> Int
getSize = Store.getSizeWith size
{-# INLINE getSize #-}

decodeFull :: Bi a => ByteString -> Either Text a
decodeFull = over _Left Store.peekExMessage . Store.decodeWith get

label :: Text -> Peek a -> Peek a
label msg p = Peek $ \pstate ptr ->
    runPeek p pstate ptr `catch` onPeekEx
  where
    onPeekEx (PeekException offset msgEx) =
        throwM (PeekException offset (msgEx <> "\n" <> msg))

instance Monoid a => Monoid (Poke a) where
    mempty = pure mempty
    m1 `mappend` m2 = Poke $ \ps off -> do
        (off1, _) <- runPoke m1 ps off
        (off2, res) <- runPoke m2 ps off1
        pure (off2, res)

----------------------------------------------------------------------------
-- Poke with Size
----------------------------------------------------------------------------

-- | A wrapper around Poke, needed for putWithLength-like functions.
data PokeWithSize a
    = PokeWithSize {
      pwsToPoke :: !(Poke a)
    , pwsToSize :: !Word32
    } deriving (Functor)

instance Applicative PokeWithSize where
    pure x = PokeWithSize (pure x) 0
    {-# INLINE pure #-}
    PokeWithSize f fsz <*> PokeWithSize v vsz = PokeWithSize (f <*> v) (fsz + vsz)
    {-# INLINE (<*>) #-}
    PokeWithSize f fsz *> PokeWithSize v vsz = PokeWithSize (f *> v) (fsz + vsz)
    {-# INLINE (*>) #-}

pokeWithSize :: Bi a => a -> PokeWithSize ()
pokeWithSize x = PokeWithSize (put x) (fromIntegral $ getSize x)

----------------------------------------------------------------------------
-- Raw
----------------------------------------------------------------------------

-- | A wrapper over 'ByteString' for adding type safety to
-- 'Pos.Crypto.Pki.encryptRaw' and friends.
--
-- TODO: maybe it should be in "Pos.Crypto"?
newtype Raw = Raw ByteString
    deriving (Bi, Eq, Ord, Show, Typeable, NFData)

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

getUnsignedVarIntSize :: (Integral a, Bits a, FiniteBits a) => a -> Int
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
-- you want to be extra careful. Compared to 'SignedVarInt' and
-- 'UnsignedVarInt', it provides two benefits:
--
-- * It is guaranteed to take either 1 or 2 bytes (the standard decoder for
--   varints can consume an unlimited amount of bytes).
--
-- * It is unambiguous (e.g. @0@ can be encoded in only one way instead of
--   two).
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

----------------------------------------------------------------------------
-- Popular basic instances
----------------------------------------------------------------------------

-- TODO get rid of boilerplate (or rewrite by hands to make it more clear)
-- I just copied most of it from here:
-- https://hackage.haskell.org/package/binary-0.8.4.1/docs/src/Data.Binary.Class.html#line-564


----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance Bi () where
    put ()  = pure ()
    get     = pure ()
    size = ConstSize 0

instance Bi Bool where
    put False = putWord8 0
    put True  = putWord8 1
    get       = getWord8 >>= toBool
      where
        toBool 0 = return False
        toBool 1 = return True
        toBool c = fail ("Could not map value " ++ show c ++ " to Bool")
    size = ConstSize 1
{-

instance Bi Char where
    {-# INLINE put #-}
    put = putCharUtf8
    get = do
        let getByte = (fromIntegral :: Word8 -> Int) <$> get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- xor 0x80 <$> getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- xor 0x80 <$> getByte
                                    y <- xor 0x80 <$>  getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- xor 0x80 <$> getByte
                                y <- xor 0x80 <$> getByte
                                z <- xor 0x80 <$> getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        getChr r
      where
        getChr w
          | w <= 0x10ffff = return $! toEnum $ fromEnum w
          | otherwise = fail "Not a valid Unicode code point!"

-}

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

instance Bi Integer where -- FIXME: CSL-1122 fixed endianness
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word8 where -- FIXME: CSL-1122 fixed endianness
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word16 where -- FIXME: CSL-1122 fixed endianness
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word32 where -- FIXME: CSL-1122 fixed endianness
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Word64 where -- FIXME: CSL-1122 fixed endianness
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Int32 where -- FIXME: CSL-1122 fixed endianness
    size = Store.size
    put = Store.poke
    get = Store.peek

getWord8 :: Peek Word8
getWord8 = get @Word8

putWord8 :: Word8 -> Poke ()
putWord8 = put @Word8

putWord8WithSize :: Word8 -> PokeWithSize ()
putWord8WithSize = pokeWithSize @Word8

----------------------------------------------------------------------------
-- Tagged
----------------------------------------------------------------------------

instance Bi a => Bi (Tagged s a) where
    put (Tagged a) = put a
    get = Tagged <$> get
    size = sizeOf unTagged

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a, b) where
    {-# INLINE size #-}
    size = combineSize (fst, snd)
    {-# INLINE put #-}
    put (a, b) = put a *> put b
    {-# INLINE get #-}
    get = liftM2 (,) get get

instance (Bi a, Bi b, Bi c) => Bi (a, b, c) where
    {-# INLINE size #-}
    size = combineSize (view _1, view _2, view _3)
    {-# INLINE put #-}
    put (a, b, c) = put a *> put b *> put c
    {-# INLINE get #-}
    get = liftM3 (,,) get get get

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a, b, c, d) where
    {-# INLINE size #-}
    size = combineSize (view _1, view _2, view _3, view _4)
    {-# INLINE put #-}
    put (a, b, c, d) = put a *> put b *> put c *> put d
    {-# INLINE get #-}
    get = liftM4 (,,,) get get get get

-- Copy-pasted from
-- https://github.com/fpco/store/blob/master/src/Data/Store/Internal.hs#L378-L389
instance Bi ByteString where
    size = VarSize $ \x ->
        let l = BS.length x in
        getSize (UnsignedVarInt l) + l
    put x = do
        let (sourceFp, sourceOffset, sourceLength) = BS.toForeignPtr x
        put $ UnsignedVarInt sourceLength
        Store.pokeFromForeignPtr sourceFp sourceOffset sourceLength
    get = do
        UnsignedVarInt len <- get
        fp <- Store.peekToPlainForeignPtr "Data.ByteString.ByteString" len
        return (BS.PS fp 0 len)

instance Bi LByteString where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance Bi Text where
    size = Store.size
    put = Store.poke
    get = Store.peek

instance KnownNat n => Bi (StaticSize n ByteString) where
    size = Store.size
    put = Store.poke
    get = Store.peek

constSize :: forall a . Bi a => Int
constSize =  case size :: Size a of
  VarSize   _ -> error "constSize: VarSize"
  ConstSize a -> a

execPoke :: Poke a -> Store.PokeState -> Store.Offset -> IO Store.Offset
execPoke p ptr offset = fst <$> Store.runPoke p ptr offset

mkPoke
    :: (Store.PokeState -> Store.Offset -> IO Store.Offset)
    -> Poke ()
mkPoke f = Store.Poke (\ptr offset -> (,()) <$> f ptr offset)

-- [CSL-1122] TODO: fix this instance
instance Bi a => Bi [a] where
    size =
        VarSize $ \t -> case size :: Size a of
            ConstSize n -> (n * length t) + constSize @(UnsignedVarInt Int)
            VarSize f   -> foldl' (\acc x -> acc + f x) (constSize @(UnsignedVarInt Int)) t
    put t = do
        put (UnsignedVarInt $ length t)
        mkPoke (\ptr offset ->
            foldlM (\offset' a -> execPoke (put a) ptr offset') offset t)
    get = do
      UnsignedVarInt len <- get
      replicateM len get

instance (Bi a, Bi b) => Bi (Either a b) where
    size = case (size @a, size @b) of
        (ConstSize s1, ConstSize s2) | s1 == s2 ->
            ConstSize (s1 + 1)
        _other ->
            VarSize $ \case
                Left  a -> getSize a + 1
                Right b -> getSize b + 1
    put (Left  a) = putWord8 0 *> put a
    put (Right b) = putWord8 1 *> put b
    get = do
        w <- getWord8
        case w of
            0 -> Left  <$> get
            1 -> Right <$> get
            _ -> fail "unexpected Either tag"


{-
-- | 'getMany n' get 'n' elements in order, without blowing the stack.
getMany :: Bi a => Int -> Get [a]
getMany n = go [] n
 where
    go xs 0 = return $! reverse xs
    go xs i = do x <- get
                 -- we must seq x to avoid stack overflows due to laziness in
                 -- (>>=)
                 x `seq` go (x:xs) (i-1)
{-# INLINE getMany #-}
-}

-- Instances for tuples with up to 5 elements are provided. There's a TH
-- generator in the neongreen/THBUG branch, but it doesn't work because,
-- well, there's a bug in TH.
--
-- [CSL-1122] TODO: benchmark 'sizeAddField' and 'combineSize' and either
-- document the difference or remove 'combineSize' if there's no difference
class CombineSize a b | a -> b where
    -- | If you have a record with fields @A {a, b, c}@ and you want to write
    -- a 'Bi' instance for it, you can write @size = combineSize (a, b, c)@.
    --
    -- If you have more than five fields, please write the following (which
    -- is probably a bit slower):
    --
    -- @ConstSize 0 `sizeAddField` a `sizeAddField` b `sizeAddField` ...@.
    combineSize :: a -> Size b

-- this could be written as “CombineSize (xt -> p1, xt -> p2) xt”, but the
-- way we do it here leads to better type inference because this instance
-- guarantees that it's the *only* possible instance for a tuple of length 2
instance (Bi a, xa ~ (x -> a),
          Bi b, xb ~ (x -> b))
         => CombineSize (xa, xb) x where
    combineSize (a, b) = Store.combineSizeWith a b size size
    {-# INLINE combineSize #-}

instance (Bi p1, x1 ~ (xt -> p1),
          Bi p2, x2 ~ (xt -> p2),
          Bi p3, x3 ~ (xt -> p3))
        => CombineSize (x1, x2, x3) xt where
   combineSize (f1, f2, f3) =
       case (size :: Size p1,
             size :: Size p2,
             size :: Size p3) of
           (ConstSize s1,
            ConstSize s2,
            ConstSize s3) -> ConstSize (s1 + s2 + s3)
           _ -> VarSize $ \xv -> getSize (f1 xv) +
                                 getSize (f2 xv) +
                                 getSize (f3 xv)
   {-# INLINE combineSize #-}

instance (Bi p1, x1 ~ (xt -> p1),
          Bi p2, x2 ~ (xt -> p2),
          Bi p3, x3 ~ (xt -> p3),
          Bi p4, x4 ~ (xt -> p4))
        => CombineSize (x1, x2, x3, x4) xt where
   combineSize (f1, f2, f3, f4) =
       case (size :: Size p1,
             size :: Size p2,
             size :: Size p3,
             size :: Size p4) of
           (ConstSize s1,
            ConstSize s2,
            ConstSize s3,
            ConstSize s4) -> ConstSize (s1 + s2 + s3 + s4)
           _ -> VarSize $ \xv -> getSize (f1 xv) +
                                 getSize (f2 xv) +
                                 getSize (f3 xv) +
                                 getSize (f4 xv)
   {-# INLINE combineSize #-}

instance (Bi p1, x1 ~ (xt -> p1),
          Bi p2, x2 ~ (xt -> p2),
          Bi p3, x3 ~ (xt -> p3),
          Bi p4, x4 ~ (xt -> p4),
          Bi p5, x5 ~ (xt -> p5))
        => CombineSize (x1, x2, x3, x4, x5) xt where
   combineSize (f1, f2, f3, f4, f5) =
       case (size :: Size p1,
             size :: Size p2,
             size :: Size p3,
             size :: Size p4,
             size :: Size p5) of
           (ConstSize s1,
            ConstSize s2,
            ConstSize s3,
            ConstSize s4,
            ConstSize s5) -> ConstSize (s1 + s2 + s3 + s4 + s5)
           _ -> VarSize $ \xv -> getSize (f1 xv) +
                                 getSize (f2 xv) +
                                 getSize (f3 xv) +
                                 getSize (f4 xv) +
                                 getSize (f5 xv)
   {-# INLINE combineSize #-}

sizeAddField :: forall a x. Bi a => Size x -> (x -> a) -> Size x
sizeAddField sizeX toA =
    case (sizeX, size @a) of
        (VarSize f, VarSize g)     -> VarSize (\x -> f x + g (toA x))
        (VarSize f, ConstSize m)   -> VarSize (\x -> f x + m)
        (ConstSize n, VarSize g)   -> VarSize (\x -> n + g (toA x))
        (ConstSize n, ConstSize m) -> ConstSize (n + m)
{-# INLINE sizeAddField #-}

infixl 9 `sizeAddField`

convertSize :: (a -> b) -> Size b -> Size a
convertSize = contramap

sizeOf :: Bi a => (x -> a) -> Size x
sizeOf conv = convertSize conv size

instance Bi a => Bi (NonEmpty a) where
    get = maybe (fail "Empty list") pure . nonEmpty =<< get
    put = put . toList
    size = sizeOf toList

instance (Bi a) => Bi (Maybe a) where
    size = VarSize $ \case
              Just x -> 1 + getSize x
              _ -> 1
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 *> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> Just <$> get

instance (Hashable k, Eq k, Bi k, Bi v) => Bi (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList
    size = sizeOf HM.toList

instance (Hashable k, Eq k, Bi k) => Bi (HashSet k) where
    get = fmap HS.fromList get
    put = put . HS.toList
    size = sizeOf HS.toList

instance (Ord k, Bi k) => Bi (Set k) where
    get = S.fromList <$> get
    put = put . S.toList
    size = sizeOf S.toList

-- Copy-pasted w/ modifications, license:
-- https://github.com/bos/vector-binary-instances/blob/master/LICENSE

instance Bi a => Bi (V.Vector a) where
    get = do
        UnsignedVarInt n <- get
        v <- pure $ unsafePerformIO $ GM.unsafeNew n
        let go 0 = return ()
            go i = do
                x <- get
                () <- pure $ unsafePerformIO $ GM.unsafeWrite v (n-i) x
                go (i-1)
        () <- go n
        pure $ unsafePerformIO $ G.unsafeFreeze v
    put v = do
        put (UnsignedVarInt (G.length v))
        G.mapM_ put v
    size = VarSize $ \v ->
        (getSize (UnsignedVarInt (G.length v))) +
        G.foldl' (\a b -> a + getSize b) 0 v

instance Bi Void where
    put = absurd
    get = fail "instance Bi Void: you shouldn't try to deserialize Void"
    size = error "instance Bi Void: you shouldn't try to serialize Void"

----------------------------------------------------------------------------
-- Other types
----------------------------------------------------------------------------

instance Bi Millisecond where
    put = put . toInteger
    get = fromInteger <$> get
    size = sizeOf toInteger

instance Bi Microsecond where
    put = put . toInteger
    get = fromInteger <$> get
    size = sizeOf toInteger

instance Bi Byte where
    put = put . toBytes
    get = fromBytes <$> get
    size = sizeOf toBytes

-- | Test Peek on empty.
isEmptyPeek :: Peek Bool
isEmptyPeek = Peek $ \end ptr ->
    pure (PeekResult ptr (ptr >= Store.peekStateEndPtr end))

-- | Try to read @a@
lookAhead :: Peek a -> Peek a
lookAhead m = Peek $ \end ptr -> Store.runPeek m end ptr `catch` onEx
  where
    onEx (PeekException ptr exMsg) =
        throwM $ PeekException ptr (exMsg <> "\nlookAhead failed")

-- | Like 'isolate', but allows consuming less bytes than expected (just not
-- more).
-- Differences from `Store.isolate`:
--  * safely handles `Int64` length argument
--  * advances pointer only by bytes read
{-# INLINE limitGet #-}
limitGet :: Int64 -> Peek a -> Peek a
limitGet len m = Peek $ \ps ptr -> do
    let end = Store.peekStateEndPtr ps
        remaining = end `minusPtr` ptr
        len' = fromIntegral $ min (fromIntegral remaining) len
        ptr2 = ptr `plusPtr` len'
    PeekResult ptr' x <- Store.runPeek m ps ptr
    when (ptr' > ptr2) $
        throwM $ PeekException (ptr' `minusPtr` ptr2) "Overshot end of isolated bytes"
    return $ PeekResult ptr' x

-- | Isolate the input to n bytes, skipping n bytes forward. Fails if @m@
-- advances the offset beyond the isolated region.
-- Differences from `Store.isolate`:
--  * safely handles `Int64` length argument
--  * requires isolated input to be fully consumed
{-# INLINE isolate64Full #-}
isolate64Full :: Int64 -> Peek a -> Peek a
isolate64Full len m = Peek $ \ps ptr -> do
    let end = Store.peekStateEndPtr ps
        remaining = end `minusPtr` ptr
    when (len > fromIntegral remaining) $
      -- Do not perform the check on the new pointer, since it could have overflowed
#if (WORD_SIZE_IN_BITS >= 64)
      Store.tooManyBytes (fromIntegral len) remaining "isolate64"
#else
      (if len <= (maxBound :: Int)
         then Store.tooManyBytes (fromIntegral len) remaining "isolate64"
         else throwM $ PeekException 0 "")
#endif
    PeekResult ptr' x <- Store.runPeek m ps ptr
    let ptr2 = ptr `plusPtr` fromIntegral len
    when (ptr' < ptr2) $
        throwM $ PeekException (ptr2 `minusPtr` ptr') "Not all isolated bytes read"
    when (ptr' > ptr2) $
        throwM $ PeekException (ptr' `minusPtr` ptr2) "Overshot end of isolated bytes"
    return $ PeekResult ptr2 x

----------------------------------------------------------------------------
-- SafeCopy
----------------------------------------------------------------------------

-- | A helper for "Data.SafeCopy" that creates 'putCopy' given a 'Bi'
-- instance.
putCopyBi :: Bi a => a -> Contained Cereal.Put
putCopyBi x = contain $ safePut (encode x)

-- | A helper for "Data.SafeCopy" that creates 'getCopy' given a 'Bi'
-- instance.
getCopyBi :: Bi a => Text -> Contained (Cereal.Get a)
getCopyBi typeName = contain $ do
    bs <- safeGet
    case decodeFull bs of
        Left err -> (fail . T.unpack) ("getCopy@" <> typeName <> ": " <> err)
        Right x  -> return x

----------------------------------------------------------------------------
-- Binary serialization
----------------------------------------------------------------------------

-- | See `Pos.Crypto.SerTypes` for details on this types
newtype AsBinary a = AsBinary
    { getAsBinary :: ByteString
    } deriving (Show, Eq, Ord, Hashable, NFData)

instance SafeCopy (AsBinary a) where
    getCopy = contain $ AsBinary <$> safeGet
    putCopy = contain . safePut . getAsBinary

class AsBinaryClass a where
    asBinary :: a -> AsBinary a
    fromBinary :: AsBinary a -> Either Text a

fromBinaryM :: (AsBinaryClass a, MonadFail m) => AsBinary a -> m a
fromBinaryM = either (fail . T.unpack) return . fromBinary

----------------------------------------------------------------------------
-- Serialization with length
----------------------------------------------------------------------------

-- | Serialize something together with its length in bytes. The length comes
-- first. If you want to serialize several things at once, use a tuple.
putWithLength :: PokeWithSize a -> Poke a
putWithLength a = put (UnsignedVarInt $ pwsToSize a) *> pwsToPoke a

-- | Read length in bytes and then parse something (which has to have exactly
-- that length).
getWithLength :: Peek a -> Peek a
getWithLength getter = do
    -- We limit the int to 20 bytes because an UnsignedVarInt Int64 takes at
    -- most 10 bytes. (20 and not 10 because it doesn't hurt to be cautious.)
    UnsignedVarInt (len :: Int64) <- limitGet 20 get
    isolate64Full len getter

-- | Read length in bytes, check that it's not bigger than a specified limit,
-- and then parse something (which has to have exactly the parsed length).
getWithLengthLimited :: Int64 -> Peek a -> Peek a
getWithLengthLimited lim getter = do
    UnsignedVarInt (len :: Int64) <- limitGet 20 get
    if len <= lim
        then isolate64Full len getter
        else fail $ formatToString
                      ("getWithLengthLimited: data ("%int%" bytes) is "%
                       "bigger than the limit ("%int%" bytes)")
                      len lim

-- | Like 'putWithLength', but should only be used for things that take less
-- than @2^14@ bytes.
--
-- Uses 'TinyVarInt' for storing length, thus guaranteeing that it won't take
-- more than 2 bytes and won't be ambiguous.
putSmallWithLength :: PokeWithSize a -> Poke a
putSmallWithLength (PokeWithSize pk len) = do
    if len >= 2^(14::Int)
        then error ("putSmallWithLength: length is " <> show len <>
                    ", but maximum allowed is 16383 (2^14-1)")
        else put (TinyVarInt (fromIntegral len)) *> pk

-- | Like 'getWithLength' but for 'putSmallWithLength'.
getSmallWithLength :: Peek a -> Peek a
getSmallWithLength getter = do
    TinyVarInt len <- get
    isolate64Full (fromIntegral len) getter

{-
----------------------------------------------------------------------------
-- Other binary utils
----------------------------------------------------------------------------

getRemainingByteString :: Peek ByteString
getRemainingByteString = BSL.toStrict <$> getRemainingLazyByteString
-}

getAsciiString1b :: String -> Word8 -> Peek String
getAsciiString1b typeName limit = getWord8 >>= \sz -> do
            if sz > limit
               then fail $ typeName ++ " shouldn't be more than "
                                    ++ show limit ++ " bytes long"
               else traverse checkAscii =<< BS.unpack <$> getBytes (fromIntegral sz)
  where
    checkAscii (chr . fromIntegral -> c) =
        if isAscii c
           then return c
           else fail $ "Not an ascii symbol in " ++ typeName ++ " " ++ show c

putAsciiString1b :: String -> Poke ()
putAsciiString1b str =  putWord8 (fromIntegral $ length str)
                     >> putBytes (BS.pack $ map (fromIntegral . ord) str)

sizeAsciiString1b :: Size String
sizeAsciiString1b = VarSize $ \s -> 1 + length s

-- | Compute size of something serializable in bytes.
biSize :: Bi a => a -> Byte
biSize = fromIntegral . getSize

-- | Get bytestring with constant length.
getBytes :: Int -> Peek ByteString
getBytes i =
    reifyNat (fromIntegral i) $
        \(_ :: Proxy n) -> unStaticSize <$>
        (Store.peek :: Peek (StaticSize n ByteString))

-- | Put bytestring with constant length.
putBytes :: ByteString -> Poke ()
putBytes bs =
    reifyNat (fromIntegral $ BS.length bs) $ \(_ :: Proxy n) ->
        Store.poke (Store.toStaticSizeEx bs :: StaticSize n ByteString)

-- putByteStringWithSize :: ByteString -> PokeWithSize ()
-- putByteStringWithSize bs =
--     reifyNat (fromIntegral $ BS.length bs) $ \(_ :: Proxy n) ->
--         PokeWithSize
--             (Store.poke (Store.toStaticSizeEx bs :: StaticSize n ByteString))
--             (fromIntegral $ BS.length bs)
