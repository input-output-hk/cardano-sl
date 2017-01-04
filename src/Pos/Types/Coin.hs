{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pos.Types.Coin
       ( Coin
       , coinF
       , sumCoins

       -- * Conversions
       , mkCoin
       , unsafeGetCoin
       , coinToInteger
       , unsafeIntegerToCoin

       -- * Arithmetic operations
       , unsafeAddCoin
       , unsafeSubCoin
       , unsafeMulCoin
       , divCoin
       ) where

import           Data.Data           (Data)
import           Data.Text.Buildable (Buildable)
import qualified Data.Text.Buildable as Buildable
import           Formatting          (Format, bprint, build, int, (%))
import           Universum

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Bounded, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build (Coin n) = bprint (int%" coin(s)") n

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

mkCoin :: Word64 -> Coin
mkCoin = Coin
{-# INLINE mkCoin #-}

sumCoins :: [Coin] -> Integer
sumCoins = sum . map coinToInteger

coinToInteger :: Coin -> Integer
coinToInteger = toInteger . getCoin
{-# INLINE coinToInteger #-}

-- | Only use if you're sure there'll be no overflow.
unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin (Coin a) (Coin b)
    | res >= a && res >= b = Coin res
    | otherwise = panic "unsafeAddCoin: overflow"
  where
    res = a+b
{-# INLINE unsafeAddCoin #-}

-- | Only use if you're sure there'll be no underflow.
unsafeSubCoin :: Coin -> Coin -> Coin
unsafeSubCoin (Coin a) (Coin b)
    | a >= b = Coin (a-b)
    | otherwise = panic "unsafeSubCoin: underflow"
{-# INLINE unsafeSubCoin #-}

-- | Only use if you're sure there'll be no overflow.
unsafeMulCoin :: Integral a => Coin -> a -> Coin
unsafeMulCoin (Coin a) b
    | res <= coinToInteger (maxBound @Coin) = Coin (fromInteger res)
    | otherwise = panic "unsafeMulCoin: overflow"
  where
    res = toInteger a * toInteger b

divCoin :: Integral a => Coin -> a -> Coin
divCoin (Coin a) b = Coin (fromInteger (toInteger a `div` toInteger b))

-- | Unwraps 'Coin'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
{-# INLINE unsafeGetCoin #-}

unsafeIntegerToCoin :: Integer -> Coin
unsafeIntegerToCoin n
  | n <= coinToInteger (maxBound :: Coin) = Coin (fromInteger n)
  | otherwise = panic "unsafeIntegerToCoin: overflow"
{-# INLINE unsafeIntegerToCoin #-}
