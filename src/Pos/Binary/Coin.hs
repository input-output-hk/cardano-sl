{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ViewPatterns #-}
module Pos.Binary.Coin
    ( encode
    , decode
    ) where

import           Control.Monad   (when)
import           Data.Binary.Get (Get, getWord8)
import           Data.Bits
import           Data.Word
import           Universum

import           Pos.Types.Core  (Coin, mkCoin, unsafeGetCoin)

-- number of total coins is 45*10^9 * 10^6
--
-- 45*10^15 needs 56 bits to represent
-- 45*10^9  (integral mega coins) needs 36 bits to represent
-- 999999   (floating mega coins) needs 20 bits to represent
--
-- decimal to needed bits:
--
--   0-9 | 4 bits
--   0-99 | 7 bits
--   0-999 | 10 bits
--   0-9999 | 14 bits
--   0-99999 | 17 bits
--   0-999999 | 20 bits
--
-- coin is splitted in mega coin (10^6) and the remaining coin for serialization
--
-- 1000999 coin = 1.000999 mega coin
--
-- simple varint encoding with word64 limit. the total length of the sequence is encoded
-- in the first byte with a variable mask.
--
--   header    | mask |  spare bits | extra byte | total bits as value | serialized size
--   ==========+======+=============+============+=====================+================
--   0 xxxxxxx | 0x7f | 7 bits      | 0          | 7 bits              | 1 byte
--   10 xxxxxx | 0x3f | 6 bits      | 1          | 14 bits             | 2 bytes
--   110 xxxxx | 0x1f | 5 bits      | 2          | 21 bits             | 3 bytes
--   1110 xxxx | 0x0f | 4 bits      | 3          | 27 bits             | 4 bytes
--   11110 xxx | 0x07 | 3 bits      | 4          | 35 bits             | 5 bytes
--   111110 xx | 0x03 | 2 bits      | 5          | 42 bits             | 6 bytes
--   1111110 x | 0x01 | 1 bit       | 6          | 49 bits             | 7 bytes
--   11111110  | 0x00 | 0 bit       | 7          | 56 bits             | 8 bytes
--   11111111  | 0x00 | 0 bit       | 8          | 64 bits             | 9 bytes
--
-- Specialized to the integral part which only need 36 bits maximum:
--
--   header    | mask |  spare bits | extra byte | total bits as value | serialized size
--   ==========+======+=============+============+=====================+================
--   0 xxxxxxx | 0x7f | 7 bits      | 0          | 7 bits              | 1 byte
--   10 xxxxxx | 0x3f | 6 bits      | 1          | 14 bits             | 2 bytes
--   110 xxxxx | 0x1f | 5 bits      | 2          | 21 bits             | 3 bytes
--   1110 xxxx | 0x0f | 4 bits      | 3          | 27 bits             | 4 bytes
--   1111 xxxx | 0x0f | 4 bits      | 4          | 36 bits             | 5 bytes
--
-- And the floating part, need 20 bits to represent, encoding value from 0 to 999999:
--
--   header    | mask |  spare bits | extra byte | total bits as value | serialized size
--   ==========+======+=============+============+=====================+================
--   0 xxxxxx  | 0x7f | 7 bits      | 0          | 7  bits             | 1 byte
--   10 xxxxxx | 0x3f | 6 bits      | 1          | 14 bits             | 2 bytes
--   110 xxxxx | 0x3f | 5 bits      | 2          | 21 bits             | 3 bytes
--
-- Note: we could save one bit in the 3 bytes scheme here by considering the end of encoding
-- but we don't need it, so by not changing the scheme we can re-use the previous scheme for
-- integral as is.
--
encode :: Coin -> [Word8]
encode (unsafeGetCoin -> w) = encodeVarint mega ++ encodeVarint (reversedBase10 micros)
  where
    (mega, micros) = w `divMod` 1000000

encodeVarint :: Word64 -> [Word8]
encodeVarint w
    | w <= 0x7F         = [fromIntegral w]
    | w <= 0x3FFF       = [0x80 .|. (w .>>. 8), fromIntegral w]
    | w <= 0x1FFFFF     = [0xc0 .|. (w .>>. 16), w .>>. 8, fromIntegral w]
    | w <= 0x0FFFFFFF   = [0xe0 .|. (w .>>. 24), w .>>. 16, w .>>. 8, fromIntegral w]
    | w <= 0x0FFFFFFFFF = [0xf0 .|. (w .>>. 32), w .>>. 24, w .>>. 16, w .>>. 8, fromIntegral w]
    | otherwise         = panic "invalid encoding for integral part"

expectedContBytes :: Word64 -> Int
expectedContBytes w
    | w <= 0x7F         = 0
    | w <= 0x3FFF       = 1
    | w <= 0x1FFFFF     = 2
    | w <= 0x0FFFFFFF   = 3
    | w <= 0x0FFFFFFFFF = 4
    | otherwise         = panic "invalid encoding"

-- given the header byte, return the number of following byte,
-- and the initial accumulator
hdrToParam :: Word8 -> (Int, Word64)
hdrToParam h
    | isClear h 7 = (0, fromIntegral (h .&. 0x7f))
    | isClear h 6 = (1, fromIntegral (h .&. 0x3f))
    | isClear h 5 = (2, fromIntegral (h .&. 0x1f))
    | isClear h 4 = (3, fromIntegral (h .&. 0x0f))
    | otherwise   = (4, fromIntegral (h .&. 0x0f))

decodeVarint :: Get Word64
decodeVarint = do
    (nbBytes, acc) <- hdrToParam <$> getWord8
    conts <- replicateM nbBytes getWord8
    let val = orAndShift acc conts
    when (expectedContBytes val /= length conts) $ fail "not canonical encoding"
    return val
  where
    orAndShift acc []     = acc
    orAndShift acc (x:xs) = orAndShift ((acc `shiftL` 8) .|. fromIntegral x) xs

decode :: Get Coin
decode = do
    (mega, microsReversed) <- (,) <$> decodeVarint <*> decodeVarint
    let micros = reversedBase10 microsReversed
        w      = mega * 1000000 + micros
    if micros < 1000000 && w <= unsafeGetCoin maxBound
        then return $ mkCoin w
        else fail "coins above limit"

------------
-- utils

(.>>.) :: Word64 -> Int -> Word8
(.>>.) w n = fromIntegral (w `shiftR` n)

-- (.<<.) :: Word8 -> Int -> Word64
-- (.<<.) w n = fromIntegral w `shiftL` n

isClear :: Word8 -> Int -> Bool
isClear w bitN = not (testBit w bitN)

toBase10 :: (Integral n, Ord n, Num n) => Int -> n -> [n]
toBase10 nbDigits = loop nbDigits
  where
    loop 0 _ = []
    loop i n = r : loop (i-1) b
       where (b, r) = n `divMod` 10

fromBase10 :: Num n => [n] -> n
fromBase10 = go 0
  where go !acc []     = acc
        go !acc (x:xs) = go (acc * 10 + x) xs

reversedBase10 :: (Integral n, Ord n, Num n) => n -> n
reversedBase10 = fromBase10 . toBase10 6
