{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns        #-}
module Pos.Types.Coin
       ( coinF
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
       , applyCoinPortion
       ) where

import           Universum

import           Pos.Types.Types (Coin, CoinPortion (getCoinPortion), coinF, mkCoin,
                                  unsafeGetCoin)

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

-- | Apply CoinPortion to Coin. 'applyCoinPortion a b' is basically
-- 'round (a * b)'.
applyCoinPortion :: CoinPortion -> Coin -> Coin
applyCoinPortion (getCoinPortion -> p) (unsafeGetCoin -> c) =
    mkCoin $ round $ (realToFrac c) * p
