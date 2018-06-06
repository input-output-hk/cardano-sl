{-# LANGUAGE RankNTypes #-}
module Pos.Core.Common.Coin
       ( Coin (..)
       , mkCoin
       , checkCoin
       , coinF

       , maxCoinVal
       , sumCoins

       -- * Conversions
       , unsafeGetCoin
       , coinToInteger
       , integerToCoin
       , unsafeIntegerToCoin

       -- * Arithmetic operations
       , unsafeAddCoin
       , unsafeSubCoin
       , unsafeMulCoin
       , subCoin
       , divCoin
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import           Data.Data (Data)
import qualified Data.Text.Buildable
import           Formatting (Format, bprint, build, int, (%))

import           Pos.Util.Log.LogSafe (SecureLog)
import           Pos.Util.Util (leftToPanic)

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build (Coin n) = bprint (int%" coin(s)") n

instance Buildable (SecureLog Coin) where
    build _ = "? coin(s)"

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin maxCoinVal

-- | Maximal possible value of 'Coin'.
maxCoinVal :: Word64
maxCoinVal = 45000000000000000

-- | Makes a 'Coin' but is _|_ if that coin exceeds 'maxCoinVal'.
-- You can also use 'checkCoin' to do that check.
mkCoin :: Word64 -> Coin
mkCoin c = either error (const coin) (checkCoin coin)
  where
    coin = (Coin c)
{-# INLINE mkCoin #-}

checkCoin :: MonadError Text m => Coin -> m ()
checkCoin (Coin c)
    | c <= maxCoinVal = pure ()
    | otherwise       = throwError $ "Coin: " <> show c <> " is too large"

-- | Coin formatter which restricts type.
coinF :: Format r (Coin -> r)
coinF = build

-- | Unwraps 'Coin'. It's called “unsafe” so that people wouldn't use it
-- willy-nilly if they want to sum coins or something. It's actually safe.
unsafeGetCoin :: Coin -> Word64
unsafeGetCoin = getCoin
{-# INLINE unsafeGetCoin #-}

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
