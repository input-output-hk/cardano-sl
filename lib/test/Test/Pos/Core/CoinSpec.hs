-- | Specification of Pos.Core.Coin

module Test.Pos.Core.CoinSpec
       ( spec
       ) where

import           Universum


import           Serokell.Util (isVerFailure)
import           Test.Hspec (Expectation, Spec, anyErrorCall, describe, it, shouldBe, shouldSatisfy)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck (Property, (.||.), (===))

import qualified Pos.Arbitrary.Core as C
import qualified Pos.Core.Common as C
import           Pos.Util.Verification (runPVerify)

import           Pos.Util.QuickCheck.Property (shouldThrowException, (.=.))

spec :: Spec
spec = describe "Coin properties" $ do
    describe "Coin" $ do
        describe "Conversions" $ do
            prop unsafeIntegerCoinDesc overflowIntegerCausesError
            prop convertingCoinDesc coinToIntegralToCoin
            prop convertingWordDesc wordToCoinToWord
            prop convertingCoinIntegerDesc coinToIntegerToCoin
            prop overflowInCheckCoinErrorDesc overflowInCheckCoinError
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
        prop appliedPortionDownDesc appliedCoinPortionDown
        prop appliedPortionUpDesc appliedCoinPortionUp
        prop unsafeCoinPortionDesc overOrUnderflowDoubleCausesError

  where
    unsafeIntegerCoinDesc = "Converting an integer that is larger than 'maxCoinVal' into\
    \ a coin causes a fatal exception"
    convertingCoinDesc = "Converting a coin to an integer and this integer to a coin\
    \ changes nothing"
    convertingWordDesc = "Converting a 64-bit word into a coin and this coin to a 64-bit\
    \ word changes nothing"
    overflowInCheckCoinErrorDesc = "Pass to mkCoin more than maxCoinVal coins, mkCoin must call error"
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
    appliedPortionDownDesc = "Applying a coin portion to a coin (down) via\
    \ 'applyCoinPortionDown' and via 'floor' is the same"
    appliedPortionUpDesc = "Applying a coin portion to a coin (up) via\
    \ 'applyCoinPortionUp' and via 'ceiling' is the same"
    unsafeCoinPortionDesc = "Converting a double outside the interval [0, 1] into a\
    \ 'CoinPortion' will raise a fatal exception"

------------------------------------------------------------------------------------------
-- Coin
------------------------------------------------------------------------------------------

overflowInSumCausesError :: C.CoinPairOverflowSum -> Expectation
overflowInSumCausesError =
    shouldThrowException (uncurry C.unsafeAddCoin . C.get2CSum) anyErrorCall

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
    shouldThrowException (uncurry C.unsafeSubCoin . C.get2CSub) anyErrorCall

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
    shouldThrowException (uncurry C.unsafeMulCoin . C.get2CMul) anyErrorCall

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
    shouldThrowException (C.unsafeIntegerToCoin . C.getLargeInteger) anyErrorCall

coinToIntegralToCoin :: C.Coin -> Property
coinToIntegralToCoin = C.mkCoin . C.unsafeGetCoin .=. identity

wordToCoinToWord :: Word64 -> Property
wordToCoinToWord c = c > C.maxCoinVal .||. C.unsafeGetCoin (C.mkCoin c) === c

overflowInCheckCoinError :: Expectation
overflowInCheckCoinError =
    shouldSatisfy (runPVerify (C.UncheckedCoin (C.maxCoinVal + 1))) isVerFailure

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
    shouldThrowException (C.unsafeCoinPortionFromDouble . C.getDouble) anyErrorCall

coinPortionToDoubleToPortion :: C.CoinPortion -> Property
coinPortionToDoubleToPortion =
    C.unsafeCoinPortionFromDouble . C.coinPortionToDouble .=. identity

appliedCoinPortionDown :: (C.CoinPortion, C.Coin) -> Property
appliedCoinPortionDown =
    let applyViaRational (C.getCoinPortion -> p, C.unsafeGetCoin -> c) =
            C.mkCoin . floor $
                (toRational p / toRational C.coinPortionDenominator) *
                (toRational c)
    in uncurry C.applyCoinPortionDown .=. applyViaRational

appliedCoinPortionUp :: (C.CoinPortion, C.Coin) -> Property
appliedCoinPortionUp =
    let applyViaRational (C.getCoinPortion -> p, C.unsafeGetCoin -> c) =
            C.mkCoin . ceiling $
                (toRational p / toRational C.coinPortionDenominator) *
                (toRational c)
    in uncurry C.applyCoinPortionUp .=. applyViaRational
