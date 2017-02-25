{-# LANGUAGE ViewPatterns        #-}
module Pos.Types.Coin
       ( coinF
       , sumCoins

       -- * Conversions
       , mkCoin
       , unsafeGetCoin
       , coinToInteger
       , unsafeIntegerToCoin
       , coinPortionToDouble

       -- * Arithmetic operations
       , unsafeAddCoin
       , unsafeSubCoin
       , unsafeMulCoin
       , divCoin
       , applyCoinPortion
       ) where

import qualified Data.Text.Buildable
import           Formatting          (bprint, float, int, (%))
import           Universum

import           Pos.Types.Core      (Coin, CoinPortion (getCoinPortion), coinF,
                                      coinPortionDenominator, mkCoin, unsafeGetCoin)

sumCoins :: [Coin] -> Integer
sumCoins = sum . map coinToInteger

coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unsafeGetCoin
{-# INLINE coinToInteger #-}

-- | Only use if you're sure there'll be no overflow.
unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | res >= a && res >= b && res <= unsafeGetCoin (maxBound @Coin) = mkCoin res
    | otherwise = panic "unsafeAddCoin: overflow"
  where
    res = a+b
{-# INLINE unsafeAddCoin #-}

-- | Only use if you're sure there'll be no underflow.
unsafeSubCoin :: Coin -> Coin -> Coin
unsafeSubCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | a >= b = mkCoin (a-b)
    | otherwise = panic "unsafeSubCoin: underflow"
{-# INLINE unsafeSubCoin #-}

-- | Only use if you're sure there'll be no overflow.
unsafeMulCoin :: Integral a => Coin -> a -> Coin
unsafeMulCoin (unsafeGetCoin -> a) b
    | res <= coinToInteger (maxBound @Coin) = mkCoin (fromInteger res)
    | otherwise = panic "unsafeMulCoin: overflow"
  where
    res = toInteger a * toInteger b

divCoin :: Integral a => Coin -> a -> Coin
divCoin (unsafeGetCoin -> a) b =
    mkCoin (fromInteger (toInteger a `div` toInteger b))

unsafeIntegerToCoin :: Integer -> Coin
unsafeIntegerToCoin n
  | n <= coinToInteger (maxBound :: Coin) = mkCoin (fromInteger n)
  | otherwise = panic "unsafeIntegerToCoin: overflow"
{-# INLINE unsafeIntegerToCoin #-}

----------------------------------------------------------------------------
-- CoinPortion
----------------------------------------------------------------------------

instance Buildable CoinPortion where
    build cp@(getCoinPortion -> x) =
        bprint
            (int%"/"%int%" (≈ "%float%")")
            x
            coinPortionDenominator
            (coinPortionToDouble cp)

coinPortionToDouble :: CoinPortion -> Double
coinPortionToDouble (getCoinPortion -> x) =
    realToFrac @_ @Double x / realToFrac coinPortionDenominator
{-# INLINE coinPortionToDouble #-}

-- | Apply CoinPortion to Coin. 'applyCoinPortion a b' is basically
-- 'round (a * b)'.
applyCoinPortion :: CoinPortion -> Coin -> Coin
applyCoinPortion (coinPortionToDouble -> p) (unsafeGetCoin -> c) =
    mkCoin $ round $ (realToFrac c) * p
