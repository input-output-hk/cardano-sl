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
       , addCoin
       , subCoin
       , mulCoin
       , divCoin
       ) where

import           Universum

import           Control.Monad.Except (MonadError (throwError))
import qualified Data.Aeson as Aeson (FromJSON (..), ToJSON (..))
import           Data.Data (Data)
import           Data.SafeCopy (base, deriveSafeCopySimple)
import           Formatting (Format, bprint, build, int, (%))
import qualified Formatting.Buildable
import qualified Text.JSON.Canonical as Canonical (FromJSON (..), ReportSchemaErrors,
                     ToJSON (..))

import           Pos.Binary.Class (Bi (..))
import           Pos.Core.Genesis.Canonical ()
import           Pos.Util.Util (leftToPanic)

-- | Coin is the least possible unit of currency.
newtype Coin = Coin
    { getCoin :: Word64
    } deriving (Show, Ord, Eq, Generic, Hashable, Data, NFData)

instance Buildable Coin where
    build (Coin n) = bprint (int%" coin(s)") n

instance Bounded Coin where
    minBound = Coin 0
    maxBound = Coin maxCoinVal

instance Bi Coin where
    encode = encode . unsafeGetCoin
    decode = Coin <$> decode
    encodedSizeExpr size pxy = size (unsafeGetCoin <$> pxy)

instance Monad m => Canonical.ToJSON m Coin where
    toJSON = Canonical.toJSON @_ @Word64 . unsafeGetCoin  -- i. e. String

instance Canonical.ReportSchemaErrors m => Canonical.FromJSON m Coin where
    fromJSON = fmap Coin . Canonical.fromJSON

instance Aeson.FromJSON Coin where
    parseJSON v = mkCoin <$> Aeson.parseJSON v

instance Aeson.ToJSON Coin where
    toJSON = Aeson.toJSON . unsafeGetCoin

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
    :: (Container coins, Element coins ~ Coin)
    => coins -> Integer
sumCoins = sum . map coinToInteger . toList

coinToInteger :: Coin -> Integer
coinToInteger = toInteger . unsafeGetCoin
{-# INLINE coinToInteger #-}

-- Addition of coins. Returns 'Nothing' in case of overflow.
addCoin :: Coin -> Coin -> Maybe Coin
addCoin (unsafeGetCoin -> a) (unsafeGetCoin -> b)
    | res >= a && res >= b && res <= unsafeGetCoin (maxBound @Coin) = Just (Coin res)
    | otherwise = Nothing
  where
    res = a+b
{-# INLINE addCoin #-}

-- | Only use if you're sure there'll be no overflow.
unsafeAddCoin :: Coin -> Coin -> Coin
unsafeAddCoin a b =
    case addCoin a b of
        Just r -> r
        Nothing ->
            error $ "unsafeAddCoin: overflow when summing " <> show a <> " + " <> show b
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

-- | Multiplication between 'Coin's. Returns 'Nothing' in case of overflow.
mulCoin :: Integral a => Coin -> a -> Maybe Coin
mulCoin (unsafeGetCoin -> a) b
    | res <= coinToInteger (maxBound @Coin) = Just $ Coin (fromInteger res)
    | otherwise = Nothing
  where
    res = toInteger a * toInteger b
{-# INLINE mulCoin #-}

-- | Only use if you're sure there'll be no overflow.
unsafeMulCoin :: Integral a => Coin -> a -> Coin
unsafeMulCoin a b =
    case mulCoin a b of
         Just r  -> r
         Nothing -> error "unsafeMulCoin: overflow"
{-# INLINE unsafeMulCoin #-}

divCoin :: Integral b => Coin -> b -> Coin
divCoin (unsafeGetCoin -> a) b = Coin (a `div` fromIntegral b)
{-# INLINE divCoin #-}

integerToCoin :: Integer -> Either Text Coin
integerToCoin n
    | n < 0 = Left $ "integerToCoin: value is negative (" <> show n <> ")"
    | n <= coinToInteger (maxBound :: Coin) = pure $ Coin (fromInteger n)
    | otherwise = Left $ "integerToCoin: value is too big (" <> show n <> ")"

unsafeIntegerToCoin :: Integer -> Coin
unsafeIntegerToCoin n = leftToPanic "unsafeIntegerToCoin: " (integerToCoin n)
{-# INLINE unsafeIntegerToCoin #-}

-- Place this here to avoid TH staging issues.
deriveSafeCopySimple 0 'base ''Coin

