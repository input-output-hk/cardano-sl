-- | Specification of Pos.Types.Coin

module Test.Pos.Types.CoinSpec
       ( spec
       ) where

import           Test.Hspec              (Expectation, Selector, Spec, describe, it,
                                          shouldBe, shouldThrow)
import           Test.Hspec.QuickCheck   (prop)
import           Universum

import           Control.Exception       (AssertionFailed)
import qualified Pos.Types               as C

import           Test.Pos.Util           ((.=.), (>=.), shouldThrowException)
import           Test.QuickCheck         (Property)

spec :: Spec
spec = describe "Coin properties" $ do
    describe "Coin" $ do
        describe "Conversions" $ do
            prop unsafeIntegerCoinDesc overflowIntegerCausesError
            prop convertingCoinDesc coinToIntegralToCoin
            prop convertingWordDesc wordToCoinToWord
            prop convertingCoinIntegerDesc coinToIntegerToCoin
            prop convertingIntegerDesc integerToCoinToInteger
        describe "unsafeAddcoin" $ do
            prop unsafeAddCoinDesc overflowInSumCausesError
            prop coinAdditionDesc coinAdditionWorks
        describe "unsafeSubCoin" $ do
            prop unsafeSubCoinDesc underflowInSubCausesError
            prop coinSubtractionDesc coinSubtractionWorks
        describe "unsafeMulCoin" $ do
            prop unsafeMulCoinDesc overflowInMulCausesError
            prop coinProductDesc coinProductWorks
        describe "sumCoin" $ do
            it "returns 0 as the sum of an empty list of coins" $
                C.sumCoins [] `shouldBe` 0
            prop sumCoinsNeverNegativeDesc sumCoinsIsNeverNegative
    describe "CoinPortion" $ do
        prop portionToDoubleToPortionDesc coinPortionToDoubleToPortion
        prop appliedPortionDesc appliedCoinPortion
        prop unsafeCoinPortionDesc overOrUnderflowDoubleCausesError
        prop wordToPortionToWordDesc wordToPortionToWord
        prop portionToWordToPortionDesc portionToWordToPortion

  where
    unsafeIntegerCoinDesc = "Converting an integer that is larger than 'maxCoinVal' into\
    \ a coin causes a fatal exception"
    convertingCoinDesc = "Converting a coin to an integer and this integer to a coin\
    \ changes nothing"
    convertingWordDesc = "Converting a 64-bit word into a coin and this coin to a 64-bit\
    \ word changes nothing"
    convertingCoinIntegerDesc = "Converting a coin into an integer and this integer to a\
    \ coin changes nothing"
    convertingIntegerDesc = "Converting a nonnegative integer into a coin and this coin\
    \ to an integer changes nothing"
    unsafeAddCoinDesc = "Adding two coins whose sum causes an overflow will throw a\
    \ fatal, uncatchable exception"
    coinAdditionDesc = "Coin addition is properly defined for two coins whose sum is less\
    \ than 'maxBound :: Coin'"
    unsafeSubCoinDesc = "Subtracting a some amount of coins from a smaller amount will\
    \ raise a fatal exception"
    coinSubtractionDesc = "Coin subtraction is properly defined when the subtrahend\
    \ is less than the minuend"
    unsafeMulCoinDesc = "Multiplying two coins whose product causes an overflow will\
    \ raise a fatal exception"
    coinProductDesc = "Coin product by an integer is properly defined for a coin and an\
    \ integer whose product does not exceed 'maxBound :: Coin'"
    sumCoinsNeverNegativeDesc = "Adding a list of coins will never result in a negative\
    \ value"
    portionToDoubleToPortionDesc = "Converting a coin portion into a double and this\
    \ double to a coin portion changes nothing"
    appliedPortionDesc = "Applying a coin portion to a coin is dividing the portion by\
    \ 'coinPortionDenominator' and multiplying it by the coin's value"
    unsafeCoinPortionDesc = "Converting a double outside the interval [0, 1] into a\
    \ 'CoinPortion' will raise a fatal exception"
    wordToPortionToWordDesc = "Converting a valid 64-bit word into a coin portion and\
    \ this portion into a word changes nothing"
    portionToWordToPortionDesc = "Converting a coin portion into a 64-bit word and this\
    \ word into a portion changes nothing"

fatalException :: Selector FatalError
fatalException = const True

assertionException :: Selector AssertionFailed
assertionException = const True

------------------------------------------------------------------------------------------
-- Coin
------------------------------------------------------------------------------------------

overflowInSumCausesError :: C.CoinPairOverflowSum -> Expectation
overflowInSumCausesError =
    shouldThrowException (uncurry C.unsafeAddCoin . C.get2CSum) fatalException

coinAdditionWorks :: C.SafeCoinPairSum -> Property
coinAdditionWorks =
    let longFunction =
            C.mkCoin .
            uncurry (+) .
            bimap C.unsafeGetCoin C.unsafeGetCoin .
            C.getPairSum
    in longFunction .=. (uncurry C.unsafeAddCoin . C.getPairSum)

underflowInSubCausesError :: C.CoinPairOverflowSub -> Expectation
underflowInSubCausesError =
    shouldThrowException (uncurry C.unsafeSubCoin . C.get2CSub) fatalException

coinSubtractionWorks :: C.SafeCoinPairSub -> Property
coinSubtractionWorks =
    let longFunction =
            C.mkCoin .
            uncurry (-) .
            bimap C.unsafeGetCoin C.unsafeGetCoin .
            C.getPairSub
    in longFunction .=. (uncurry C.unsafeSubCoin . C.getPairSub)

overflowInMulCausesError :: C.CoinPairOverflowMul -> Expectation
overflowInMulCausesError =
    shouldThrowException (uncurry C.unsafeMulCoin . C.get2CMul) fatalException

coinProductWorks :: C.SafeCoinPairMul -> Property
coinProductWorks =
    let longFunction =
            C.unsafeIntegerToCoin .
            uncurry (*) .
            bimap C.coinToInteger identity .
            C.getPairMul
    in longFunction .=. (uncurry C.unsafeMulCoin . C.getPairMul)

overflowIntegerCausesError :: C.IntegerToCoinOverflow -> Expectation
overflowIntegerCausesError =
    shouldThrowException (C.unsafeIntegerToCoin . C.getLargeInteger) fatalException

coinToIntegralToCoin :: C.Coin -> Property
coinToIntegralToCoin = C.mkCoin . C.unsafeGetCoin .=. identity

wordToCoinToWord :: Word64 -> Property
wordToCoinToWord = C.unsafeGetCoin . C.mkCoin .=. identity

coinToIntegerToCoin :: C.Coin -> Property
coinToIntegerToCoin = C.unsafeIntegerToCoin . C.coinToInteger .=. identity

integerToCoinToInteger :: C.IntegerToCoinNoOverflow -> Property
integerToCoinToInteger =
    C.Integer . C.coinToInteger . C.unsafeIntegerToCoin . C.getInteger .=. identity

sumCoinsIsNeverNegative :: [C.Coin] -> Property
sumCoinsIsNeverNegative = (>= 0) . C.sumCoins .=. const True

------------------------------------------------------------------------------------------
-- CoinPortion
------------------------------------------------------------------------------------------

overOrUnderflowDoubleCausesError :: C.LessThanZeroOrMoreThanOne -> Expectation
overOrUnderflowDoubleCausesError =
    shouldThrowException (C.unsafeCoinPortionFromDouble . C.getDouble) fatalException

coinPortionToDoubleToPortion :: C.CoinPortion -> Property
coinPortionToDoubleToPortion =
    C.unsafeCoinPortionFromDouble . C.coinPortionToDouble .=. identity

-- | Combinator to facilitate the 'wordToPortion' property.
(=<<<) :: Monad m => (a -> b) -> (c -> m a) -> (c -> m b)
(=<<<) f g = \c -> do
    a <- g c
    return . f $ a

infixr 5 =<<<

wordToPortionToWord :: C.SafeWord -> Property
wordToPortionToWord =
    (C.SafeWord . C.getCoinPortion =<<< C.mkCoinPortion . C.getSafeWord) >=. (pure @Maybe)

portionToWordToPortion :: C.CoinPortion -> Property
portionToWordToPortion = C.mkCoinPortion . C.getCoinPortion >=. (pure @Maybe)

appliedCoinPortion :: (C.CoinPortion, C.Coin) -> Property
appliedCoinPortion =
    let longFunction =
            C.mkCoin .
            round .
            uncurry (*) .
            bimap ((/ realToFrac C.coinPortionDenominator) .
                   realToFrac @_ @Double .
                   C.getCoinPortion)
                  (realToFrac .
                   C.unsafeGetCoin)
    in uncurry C.applyCoinPortion .=. longFunction
