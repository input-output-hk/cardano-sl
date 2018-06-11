-- | Basic utilities to convert back an forth to binary representation.
-- Note that conversions functions from [Bit] to anything all assume the right
-- number of bits. They're partial functions and should be used with care.

module Pos.Util.Bits
    (
    -- * Types
      Bit
    , Word11

    -- * Conversions helper for list of @Word8@ or @Word11@
    , FromBits(..)
    , ToBits(..)

    -- * Constructor, mostly for testing
    , one
    , zero
    ) where

import           Universum hiding (one)

import           Data.Bits (Bits, shiftL, shiftR, (.&.))
import           Formatting (bprint, build)

import qualified Data.Text.Buildable


-- Type Alias for readability. A bit can only be 1 or 0, but we
-- use Word8 instead of Bool to leverage some available type-classes.
newtype Bit
    = Bit Word8
    deriving (Show, Eq)

instance Buildable Bit where
    build (Bit b) =
        bprint build b


-- Type Alias for readability
newtype Word11
    = Word11 Int
    deriving (Show, Eq, Ord, Enum, Num, Real, Integral, Bits)

instance Bounded Word11 where
    minBound = 0
    maxBound = 2047

one :: Bit
one = Bit 1

zero :: Bit
zero = Bit 0

class (Bits a, Num a, Integral a) => FromBits a where
    fromBits :: [Bit] -> Either Text [a]

class (Bits a, Num a, Integral a) => ToBits a where
    toBits :: [a] -> [Bit]


instance FromBits Word8 where
    fromBits =
        mapM (bitsToWordN 8) . groupOf 8

instance ToBits Word8 where
    toBits =
        concatMap (wordNToBits 8)

instance FromBits Word11 where
    fromBits =
        mapM (bitsToWordN 11) . groupOf 11

instance ToBits Word11 where
    toBits =
        concatMap (wordNToBits 11)


--
-- Internals
--

-- | Makes groups of @size@ elements from a list. Not that it doesn't insure
-- that there are enough elements to make equal groups in the list.
groupOf :: Int -> [a] -> [[a]]
groupOf =
    iter []
  where
    iter acc _ [] = acc
    iter acc size xs =
        let
            (h, q) = splitAt size xs
        in
            iter (acc ++ [h]) size q


wordNToBits :: (Bits a, Integral a) => Int -> a -> [Bit]
wordNToBits e0 =
    iter e0 []
  where
    iter 0 xs _ = xs
    iter e xs n =
        let
            e'  = e - 1
            d   = 1 `shiftL` e'
            bit = (n .&. d) `shiftR` e'
        in
            iter e' (xs ++ [Bit $ fromIntegral bit]) (n - bit * d)
{-# INLINE wordNToBits #-}


bitsToWordN :: (Bits a, Num a) => Int -> [Bit] -> Either Text a
bitsToWordN e0 =
    iter e0 0
  where
    iter 0 wrd [] = Right wrd
    iter e wrd ((Bit bit) : q) =
        let
            e' = e - 1
            d  = 1 `shiftL` e'
        in
            iter e' (wrd + fromIntegral bit * d) q
    iter _ _ _ =
        Left $ "bitsToWord" <> show e0 <> ": wrong number of bits"
{-# INLINE bitsToWordN #-}
