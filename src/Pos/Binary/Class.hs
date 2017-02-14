{-# LANGUAGE BinaryLiterals       #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE RankNTypes           #-}

#include "MachDeps.h"

{-# OPTIONS_GHC -Wno-redundant-constraints #-}

-- | Serialization-related types

module Pos.Binary.Class
       ( Bi (..)
       , encode
       , encodeStrict
       , decode
       , decodeOrFail
       , decodeFull

       -- * Different sizes for ints
       , UnsignedVarInt(..)
       , SignedVarInt(..)
       , TinyVarInt(..)
       , FixedSizeInt(..)

       -- * Primitives for limiting serialization
       , limitGet
       , isolate64
       ) where

import           Data.Binary                 (Get, Put)
import qualified Data.Binary                 as Binary
import           Data.Binary.Get             (ByteOffset, getByteString,
                                              getLazyByteString, getWord8, runGet,
                                              label, runGetOrFail)
import           Data.Binary.Get.Internal    (Decoder (..), runCont)
import           Data.Binary.Put             (putByteString, putCharUtf8,
                                              putLazyByteString, putWord8, runPut)
import qualified Data.ByteString             as BS
import qualified Data.ByteString.Lazy        as BSL
import           Data.Hashable               (Hashable (..))
import qualified Data.HashMap.Strict         as HM
import qualified Data.HashSet                as HS
import qualified Data.Text.Encoding          as T
import           Data.Time.Units             (Microsecond, Millisecond)
import qualified Data.Vector                 as V
import qualified Data.Vector.Generic         as G
import qualified Data.Vector.Generic.Mutable as GM
import           Data.Word                   (Word32)
import           GHC.TypeLits                (ErrorMessage (..), TypeError)
import           Serokell.Data.Memory.Units  (Byte, fromBytes, toBytes)
import           System.IO.Unsafe            (unsafePerformIO)
import           Universum                   hiding (putByteString)
import           Unsafe.Coerce               (unsafeCoerce)

----------------------------------------------------------------------------
-- Bi typeclass
----------------------------------------------------------------------------

-- | Simplified definition of serializable object,
-- Data.Binary.Class-alike.
--
-- Write @instance Bi SomeType where@ without any method definitions if you
-- want to use the 'Binary' instance for your type.
class Bi t where
    put :: t -> Put
    default put :: Binary.Binary t => t -> Put
    put = Binary.put
    {-# INLINE put #-}

    get :: Get t
    default get :: Binary.Binary t => Get t
    get = Binary.get
    {-# INLINE get #-}

--instance Serializable t => B.Binary t where
--    get = get
--    put = put

-- | Encode a value to a lazy bytestring
encode :: Bi a => a -> LByteString
encode = runPut . put
{-# INLINE encode #-}

-- | Encode a value to a strict bytestring.  Use with caution, because
-- converting to strict ByteString is expensive.
encodeStrict :: Bi a => a -> ByteString
encodeStrict = BSL.toStrict . encode
{-# INLINE encodeStrict #-}

-- | Decode a value from a lazy ByteString, reconstructing the
-- original structure.
decode :: Bi a => LByteString -> a
decode = runGet get

decodeOrFail
    :: Bi a
    => LByteString
    -> Either (LByteString, ByteOffset, String)
              (LByteString, ByteOffset, a)
decodeOrFail = runGetOrFail get

-- | Like 'decode', but ensures that the whole input has been consumed.
decodeFull :: Bi a => LByteString -> Either String a
decodeFull bs = case (runGetOrFail get) bs of
    Left (_, _, err) -> Left ("decodeFull: " ++ err)
    Right (unconsumed, _, a)
        | BSL.null unconsumed -> Right a
        | otherwise -> Left "decodeFull: unconsumed input"

----------------------------------------------------------------------------
-- Variable-sized numbers
----------------------------------------------------------------------------

-- Copied from Edward Kmett's 'bytes' library (licensed under BSD3)
putUnsignedVarInt :: (Integral a, Bits a) => a -> Put
putUnsignedVarInt n
    | n < 0x80 = putWord8 $ fromIntegral n
    | otherwise = do
          putWord8 $ setBit (fromIntegral n) 7
          putUnsignedVarInt $ shiftR n 7
{-# INLINE putUnsignedVarInt #-}

getUnsignedVarInt' :: (Num a, Bits a) => Get a
getUnsignedVarInt' = getWord8 >>= go
  where
    go n | testBit n 7 = do
             m <- getWord8 >>= go
             return $ shiftL m 7 .|. clearBit (fromIntegral n) 7
         | otherwise = return $ fromIntegral n
{-# INLINE getUnsignedVarInt' #-}

putSignedVarInt :: (ZZEncode a b, Integral b, Bits b) => a -> Put
putSignedVarInt x = putUnsignedVarInt (zzEncode x)
{-# INLINE putSignedVarInt #-}

getSignedVarInt' :: (ZZEncode a b, Num b, Bits b) => Get a
getSignedVarInt' = zzDecode <$> getUnsignedVarInt'
{-# INLINE getSignedVarInt' #-}

putTinyVarInt :: Word16 -> Put
putTinyVarInt n
    | n <= 0b1111111 =
          putWord8 (fromIntegral n)
    | n <= 0b11111111111111 =
          putWord8 (setBit (fromIntegral n) 7) <>
          putWord8 (fromIntegral (shiftR n 7))
    | otherwise =
          panic "putTinyVarInt: the number is bigger than 2^14-1"

getTinyVarInt' :: Get Word16
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
    get = panic "get@Int"
    put = panic "put@Int"

instance TypeError
    ('Text "Do not encode 'Word' directly. Instead, use one of newtype wrappers:" ':$$:
     'Text "  'FixedSizeInt': always uses 8 bytes" ':$$:
     'Text "  'UnsignedVarInt': uses 1–10 bytes (1 byte for 0..127)")
  => Bi Word
  where
    get = panic "get@Word"
    put = panic "put@Word"

-- Int

instance Bi (UnsignedVarInt Int) where
    put (UnsignedVarInt a) = put (UnsignedVarInt (fromIntegral a :: Int64))
    {-# INLINE put #-}
    -- We don't need 'limitGet' here because it's already present in the 'Bi'
    -- instance of @UnsignedVarInt Int64@
    get = label "UnsignedVarInt Int" $
        fmap (fromIntegral :: Int64 -> Int) <$> get
    {-# INLINE get #-}

instance Bi (SignedVarInt Int) where
    put (SignedVarInt a) = putSignedVarInt a
    {-# INLINE put #-}
    get = label "SignedVarInt Int" $
        SignedVarInt <$> limitGet 15 getSignedVarInt'
    {-# INLINE get #-}

instance Bi (FixedSizeInt Int) where
    put (FixedSizeInt a) = Binary.put a
    {-# INLINE put #-}
    get = label "FixedSizeInt Int" $
        FixedSizeInt <$> Binary.get
    {-# INLINE get #-}

-- Int64

instance Bi (UnsignedVarInt Int64) where
    put (UnsignedVarInt a) = putUnsignedVarInt (fromIntegral a :: Word64)
    {-# INLINE put #-}
    get = label "UnsignedVarInt Int64" $
        UnsignedVarInt . (fromIntegral :: Word64 -> Int64) <$>
        limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}

instance Bi (SignedVarInt Int64) where
    put (SignedVarInt a) = putSignedVarInt a
    {-# INLINE put #-}
    get = label "SignedVarInt Int64" $
        SignedVarInt <$> limitGet 15 getSignedVarInt'
    {-# INLINE get #-}

instance Bi (FixedSizeInt Int64) where
    put (FixedSizeInt a) = Binary.put a
    {-# INLINE put #-}
    get = label "FixedSizeInt Int64" $
        FixedSizeInt <$> Binary.get
    {-# INLINE get #-}

-- Word

instance Bi (UnsignedVarInt Word) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}

instance Bi (FixedSizeInt Word) where
    put (FixedSizeInt a) = Binary.put a
    {-# INLINE put #-}
    get = label "FixedSizeInt Word" $
        FixedSizeInt <$> Binary.get
    {-# INLINE get #-}

-- Word16

instance Bi (UnsignedVarInt Word16) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word16" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}

-- Word32

instance Bi (UnsignedVarInt Word32) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word32" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}

-- Word64

instance Bi (UnsignedVarInt Word64) where
    put (UnsignedVarInt a) = putUnsignedVarInt a
    {-# INLINE put #-}
    get = label "UnsignedVarInt Word64" $
        UnsignedVarInt <$> limitGet 15 getUnsignedVarInt'
    {-# INLINE get #-}

-- TinyVarInt

instance Bi TinyVarInt where
    put (TinyVarInt a) = putTinyVarInt a
    {-# INLINE put #-}
    -- Doesn't need 'limitGet' because 'TinyVarInt' is already limited to two
    -- bytes
    get = label "TinyVarInt" $
        TinyVarInt <$> getTinyVarInt'
    {-# INLINE get #-}

----------------------------------------------------------------------------
-- Popular basic instances
----------------------------------------------------------------------------

-- TODO get rid of boilerplate (or rewrite by hands to make it more clear)
-- I just copied most of it from here:
-- https://hackage.haskell.org/package/binary-0.8.4.1/docs/src/Data.Binary.Class.html#line-564

{-
Copyright (c) Lennart Kolmodin

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the distribution.

3. Neither the name of the author nor the names of his contributors
   may be used to endorse or promote products derived from this software
   without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE CONTRIBUTORS ``AS IS'' AND ANY EXPRESS
OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
POSSIBILITY OF SUCH DAMAGE.
-}

----------------------------------------------------------------------------
-- Primitive types
----------------------------------------------------------------------------

instance Bi () where
    put ()  = mempty
    get     = return ()

instance Bi Bool where
    put     = putWord8 . fromIntegral . fromEnum
    get     = getWord8 >>= toBool
      where
        toBool 0 = return False
        toBool 1 = return True
        toBool c = fail ("Could not map value " ++ show c ++ " to Bool")

instance Bi Char where
    {-# INLINE put #-}
    put = putCharUtf8
    get = do
        let getByte = liftM (fromIntegral :: Word8 -> Int) get
            shiftL6 = flip shiftL 6 :: Int -> Int
        w <- getByte
        r <- case () of
                _ | w < 0x80  -> return w
                  | w < 0xe0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    return (x .|. shiftL6 (xor 0xc0 w))
                  | w < 0xf0  -> do
                                    x <- liftM (xor 0x80) getByte
                                    y <- liftM (xor 0x80) getByte
                                    return (y .|. shiftL6 (x .|. shiftL6
                                            (xor 0xe0 w)))
                  | otherwise -> do
                                x <- liftM (xor 0x80) getByte
                                y <- liftM (xor 0x80) getByte
                                z <- liftM (xor 0x80) getByte
                                return (z .|. shiftL6 (y .|. shiftL6
                                        (x .|. shiftL6 (xor 0xf0 w))))
        getChr r
      where
        getChr w
          | w <= 0x10ffff = return $! toEnum $ fromEnum w
          | otherwise = fail "Not a valid Unicode code point!"

----------------------------------------------------------------------------
-- Numeric data
----------------------------------------------------------------------------

-- These instances just copy 'Binary'

instance Bi Integer            -- TODO: write how Integer is serialized

instance Bi Int16              -- 2 bytes, big endian
instance Bi Int32              -- 4 bytes, big endian
instance Bi Int64              -- 8 bytes, big endian

instance Bi Word8              -- single byte
instance Bi Word16             -- 2 bytes, big endian
instance Bi Word32             -- 4 bytes, big endian
instance Bi Word64             -- 8 bytes, big endian

----------------------------------------------------------------------------
-- Containers
----------------------------------------------------------------------------

instance (Bi a, Bi b) => Bi (a, b) where
    {-# INLINE put #-}
    put (a, b) = put a <> put b
    {-# INLINE get #-}
    get = liftM2 (,) get get

instance (Bi a, Bi b, Bi c) => Bi (a, b, c) where
    {-# INLINE put #-}
    put (a, b, c) = put a <> put b <> put c
    {-# INLINE get #-}
    get = liftM3 (,,) get get get

instance (Bi a, Bi b, Bi c, Bi d) => Bi (a, b, c, d) where
    {-# INLINE put #-}
    put (a, b, c, d) = put a <> put b <> put c <> put d
    {-# INLINE get #-}
    get = liftM4 (,,,) get get get get

instance Bi ByteString where
    put bs = put (UnsignedVarInt (BS.length bs)) <> putByteString bs
    get = do
        UnsignedVarInt n <- get
        getByteString n

instance Bi LByteString where
    put bs = put (UnsignedVarInt (BSL.length bs)) <> putLazyByteString bs
    get = do
        UnsignedVarInt n <- get
        getLazyByteString n

instance Bi Text where
    put t = put (T.encodeUtf8 t)
    get = do
        bs <- get
        case T.decodeUtf8' bs of
            Left e  -> fail (show e)
            Right a -> return a

instance Bi a => Bi [a] where
    put xs = put (UnsignedVarInt (length xs)) <> mapM_ put xs
    get = do UnsignedVarInt n <- get
             getMany (n :: Int)

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

instance (Bi a, Bi b) => Bi (Either a b) where
    put (Left  a) = putWord8 0 <> put a
    put (Right b) = putWord8 1 <> put b
    get = do
        w <- getWord8
        case w of
            0 -> liftM Left  get
            1 -> liftM Right get
            _ -> fail "unexpected Either tag"

instance Bi a => Bi (NonEmpty a) where
    get = maybe (fail "Empty list") pure . nonEmpty =<< get
    put = put . toList

instance (Bi a) => Bi (Maybe a) where
    put Nothing  = putWord8 0
    put (Just x) = putWord8 1 <> put x
    get = do
        w <- getWord8
        case w of
            0 -> return Nothing
            _ -> liftM Just get

instance (Hashable k, Eq k, Bi k, Bi v) => Bi (HM.HashMap k v) where
    get = fmap HM.fromList get
    put = put . HM.toList

instance (Hashable k, Eq k, Bi k) => Bi (HashSet k) where
    get = fmap HS.fromList get
    put = put . HS.toList

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

instance Bi Void where
    put = absurd
    get = mzero

----------------------------------------------------------------------------
-- Other types
----------------------------------------------------------------------------

instance Bi Millisecond where
    put = put . toInteger
    get = fromInteger <$> get

instance Bi Microsecond where
    put = put . toInteger
    get = fromInteger <$> get

instance Bi Byte where
    put = put . toBytes
    get = fromBytes <$> get

----------------------------------------------------------------------------
-- Primitives for limiting serialization
----------------------------------------------------------------------------

-- | Like 'isolate', but allows consuming less bytes than expected (just not
-- more).
limitGet :: Int64  -- ^ The upper limit on byte consumption
         -> Get a  -- ^ The decoder to isolate
         -> Get a
-- A modified version of 'isolate' from Data.Binary.Get
limitGet n0 act
  | n0 < 0 = fail "limitGet: negative size"
  | otherwise = go n0 (runCont act BS.empty Done)
  where
  go _ (Done left x) = pushFront left >> return x
  go 0 (Partial resume) = go 0 (resume Nothing)
  go n (Partial resume) = do
    inp <- unsafeCoerce (OurC (\inp k -> do
      let takeLimited str =
#if (WORD_SIZE_IN_BITS == 64)
            let (inp', out) = BS.splitAt (fromIntegral n) str
#else
            let (inp', out) = if n > fromIntegral (maxBound :: Int)
                                then (str, BS.empty)
                                else BS.splitAt (fromIntegral n) str
#endif
            in k out (Just inp')
      case not (BS.null inp) of
        True  -> takeLimited inp
        False -> prompt inp (k BS.empty Nothing) takeLimited))
    case inp of
      Nothing  -> go n (resume Nothing)
      Just str -> go (n - fromIntegral (length str)) (resume (Just str))
  go _ (Fail bs err) = pushFront bs >> fail err
  go n (BytesRead r resume) =
    go n (resume $! n0 - n - r)

-- | Like 'isolate', but works with Int64 only.
isolate64 :: Int64
          -> Get a
          -> Get a
isolate64 n0 act
  | n0 < 0 = fail "isolate64: negative size"
  | otherwise = go n0 (runCont act BS.empty Done)
  where
  go !n (Done left x)
    | n == 0 && BS.null left = return x
    | otherwise = do
        pushFront left
        let consumed = n0 - n - fromIntegral (BS.length left)
        fail $ "isolate: the decoder consumed " ++ show consumed ++ " bytes" ++
                 " which is less than the expected " ++ show n0 ++ " bytes"
  go 0 (Partial resume) = go 0 (resume Nothing)
  go n (Partial resume) = do
    inp <- unsafeCoerce (OurC (\inp k -> do
      let takeLimited str =
#if (WORD_SIZE_IN_BITS == 64)
            let (inp', out) = BS.splitAt (fromIntegral n) str
#else
            let (inp', out) = if n > fromIntegral (maxBound :: Int)
                                then (str, BS.empty)
                                else BS.splitAt (fromIntegral n) str
#endif
            in k out (Just inp')
      case not (BS.null inp) of
        True  -> takeLimited inp
        False -> prompt inp (k BS.empty Nothing) takeLimited))
    case inp of
      Nothing  -> go n (resume Nothing)
      Just str -> go (n - fromIntegral (BS.length str)) (resume (Just str))
  go _ (Fail bs err) = pushFront bs >> fail err
  go n (BytesRead r resume) =
    go n (resume $! n0 - n - r)

----------------------------------------------------------------------------
-- Guts of 'binary'
----------------------------------------------------------------------------

-- Using 'unsafeCoerce' here because 'C' isn't exported. Aargh. For now it'll
-- do and then I'll submit some pull requests to 'binary' and hopefully all
-- of this won't be needed. –@neongreen
pushFront :: ByteString -> Get ()
pushFront bs = unsafeCoerce (OurC (\ inp ks -> ks (BS.append bs inp) ()))
{-# INLINE pushFront #-}

-- This ***has*** to correspond to the implementation of 'Get' in 'binary'
-- because we're using it for 'unsafeCoerce'.
newtype OurGet a = OurC (forall r. ByteString ->
                                   OurSuccess a r ->
                                   Decoder      r )

-- Ditto.
type OurSuccess a r = ByteString -> a -> Decoder r

-- More functions from 'binary'.
prompt :: ByteString -> Decoder a -> (ByteString -> Decoder a) -> Decoder a
prompt inp kf ks = prompt' kf (\inp' -> ks (inp `BS.append` inp'))

-- And more.
prompt' :: Decoder a -> (ByteString -> Decoder a) -> Decoder a
prompt' kf ks =
  let loop =
        Partial $ \sm ->
          case sm of
            Just s | BS.null s -> loop
                   | otherwise -> ks s
            Nothing -> kf
  in loop
