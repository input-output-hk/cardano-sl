{-# LANGUAGE RankNTypes   #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pos.Core.Common.Coin
       ( coinF
       , sumCoins

       -- * Conversions
       , unsafeGetCoin
       , coinToInteger
       , integerToCoin
       , unsafeIntegerToCoin
       , coinPortionToDouble

       -- * Arithmetic operations
       , unsafeAddCoin
       , unsafeSubCoin
       , unsafeMulCoin
       , subCoin
       , divCoin
       , applyCoinPortionDown
       , applyCoinPortionUp
       ) where

import           Universum

import qualified Data.Text.Buildable
import           Formatting (bprint, float, int, (%))

import           Pos.Core.Common.Types (Coin (..), CoinPortion (..), coinF, coinPortionDenominator,
                                        unsafeGetCoin)
import           Pos.Util.Util (leftToPanic)

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
    | res >= a && res >= b && res <= unsafeGetCoin (maxBound @Coin) = Coin res
    | otherwise =
      error $ "unsafeAddCoin: overflow when summing " <> show a <> " + " <> show b
  where
    res = a+b
{-# INLINE unsafeAddCoin #-}

-- | Subtraction of coins. Returns 'Nothing' when the subtrahend is bigger
-- than the minuend, and 'Just' otherwise.
subCoin :: Coin -> Coin -> Maybe Coin
subCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | a >= b = Just (Coin (a-b))
    | otherwise = Nothing

-- | Only use if you're sure there'll be no underflow.
unsafeSubCoin :: Coin -> Coin -> Coin
unsafeSubCoin a b = fromMaybe (error "unsafeSubCoin: underflow") (subCoin a b)
{-# INLINE unsafeSubCoin #-}

-- | Only use if you're sure there'll be no overflow.
unsafeMulCoin :: Integral a => Coin -> a -> Coin
unsafeMulCoin (unsafeGetCoin -> a) b
    | res <= coinToInteger (maxBound @Coin) = Coin (fromInteger res)
    | otherwise = error "unsafeMulCoin: overflow"
  where
    res = toInteger a * toInteger b

divCoin :: Integral a => Coin -> a -> Coin
divCoin (unsafeGetCoin -> a) b =
    Coin (fromInteger (toInteger a `div` toInteger b))

integerToCoin :: Integer -> Either Text Coin
integerToCoin n
    | n < 0 = Left $ "integerToCoin: value is negative (" <> show n <> ")"
    | n <= coinToInteger (maxBound :: Coin) = pure $ Coin (fromInteger n)
    | otherwise = Left $ "integerToCoin: value is too big (" <> show n <> ")"

unsafeIntegerToCoin :: Integer -> Coin
unsafeIntegerToCoin n = leftToPanic "unsafeIntegerToCoin: " (integerToCoin n)
{-# INLINE unsafeIntegerToCoin #-}

----------------------------------------------------------------------------
-- CoinPortion
----------------------------------------------------------------------------

instance Buildable CoinPortion where
    build cp@(getCoinPortion -> x) =
        bprint
            (int%"/"%int%" (approx. "%float%")")
            x
            coinPortionDenominator
            (coinPortionToDouble cp)

coinPortionToDouble :: CoinPortion -> Double
coinPortionToDouble (getCoinPortion -> x) =
    realToFrac @_ @Double x / realToFrac coinPortionDenominator
{-# INLINE coinPortionToDouble #-}

-- | Apply CoinPortion to Coin (with rounding down).
--
-- Use it for calculating coin amounts.
applyCoinPortionDown :: CoinPortion -> Coin -> Coin
applyCoinPortionDown (getCoinPortion -> p) (unsafeGetCoin -> c) =
    Coin . fromInteger $
        (toInteger p * toInteger c) `div`
        (toInteger coinPortionDenominator)

-- | Apply CoinPortion to Coin (with rounding up).
--
-- Use it for calculating thresholds.
applyCoinPortionUp :: CoinPortion -> Coin -> Coin
applyCoinPortionUp (getCoinPortion -> p) (unsafeGetCoin -> c) =
    let (d, m) = divMod (toInteger p * toInteger c)
                        (toInteger coinPortionDenominator)
    in if m > 0 then Coin (fromInteger (d + 1))
                else Coin (fromInteger d)
