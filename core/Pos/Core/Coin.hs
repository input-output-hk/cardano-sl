{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
{-# LANGUAGE ViewPatterns        #-}

module Pos.Core.Coin
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

import           Pos.Core.Types      (Coin, CoinPortion (getCoinPortion), coinF,
                                      coinPortionDenominator, mkCoin, unsafeGetCoin)

-- | Compute sum of all coins in container. Result is 'Integer' as a
-- protection against possible overflow. If you are sure overflow is
-- impossible, you can use 'unsafeIntegerToCoin'.
sumCoins
    :: (NontrivialContainer coins, Element coins ~ Coin)
    => coins -> Integer
sumCoins = sum . map coinToInteger . toList

coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unsafeGetCoin
{-# INLINE coinToInteger #-}

-- | Only use if you're sure there'll be no overflow.
unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | res >= a && res >= b && res <= unsafeGetCoin (maxBound @Coin) = mkCoin res
    | otherwise = error "unsafeAddCoin: overflow"
  where
    res = a+b
{-# INLINE unsafeAddCoin #-}

-- | Only use if you're sure there'll be no underflow.
unsafeSubCoin :: Coin -> Coin -> Coin
unsafeSubCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | a >= b = mkCoin (a-b)
    | otherwise = error "unsafeSubCoin: underflow"
{-# INLINE unsafeSubCoin #-}

-- | Only use if you're sure there'll be no overflow.
unsafeMulCoin :: Integral a => Coin -> a -> Coin
unsafeMulCoin (unsafeGetCoin -> a) b
    | res <= coinToInteger (maxBound @Coin) = mkCoin (fromInteger res)
    | otherwise = error "unsafeMulCoin: overflow"
  where
    res = toInteger a * toInteger b

divCoin :: Integral a => Coin -> a -> Coin
divCoin (unsafeGetCoin -> a) b =
    mkCoin (fromInteger (toInteger a `div` toInteger b))

unsafeIntegerToCoin :: Integer -> Coin
unsafeIntegerToCoin n
  | n <= coinToInteger (maxBound :: Coin) = mkCoin (fromInteger n)
  | otherwise = error "unsafeIntegerToCoin: overflow"
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
