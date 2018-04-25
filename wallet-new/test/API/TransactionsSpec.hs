module API.TransactionsSpec where

import           Universum

import           Test.Hspec
import           Test.Hspec.QuickCheck
import Test.QuickCheck

import           Cardano.Wallet.API.V1.LegacyHandlers.Transactions

spec :: Spec
spec = do
    describe "rawMkLowerBound" $ do
        prop "correctly calculates for any amounts" $
            \amount available additional ->
                rawMkLowerBound
                    (TxnAmount amount)
                    (AvailableCoin available)
                    (AdditionalCoin additional)
                ===
                    (available - amount + additional)

        it "correctly calculates the amount #1" $ do
            rawMkLowerBound
                (TxnAmount 90)
                (AvailableCoin 100)
                (AdditionalCoin 10)
                `shouldBe`
                    20

        it "correctly calculates the amount #2" $ do
            rawMkLowerBound
                (TxnAmount 50)
                (AvailableCoin 100)
                (AdditionalCoin 10)
                `shouldBe`
                    60

