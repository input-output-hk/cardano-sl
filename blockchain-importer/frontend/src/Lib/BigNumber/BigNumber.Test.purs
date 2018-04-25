module BigNumber.Test where

import Prelude
import BigNumber (BIGNUMBER, BigNumberFormat(..), defaultFormat, dividedByInt, fromString, toFormat, toString, toString')
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)


testBigNumber :: forall r. StateT (Array (Group (Aff (bigNumber :: BIGNUMBER | r) Unit))) Identity Unit
testBigNumber =
    describe "BigNumber" do
        it "creates an instance of a BigNumber from a string" do
            let value = "12345678901"
                result = fromString value
            (fromMaybe "failed" $ show <$> result) `shouldEqual` value
        it "converts a BigNumber to a string by using base of 10 (default base) " do
            let value = "750000"
                result = fromMaybe "0" $ toString' <$> fromString value
            result `shouldEqual` value
        it "converts a BigNumber to a string by using base of 9 " do
            let value = "362.875"
                result = fromMaybe "0" $ toString <$> fromString value <*> Just 9
            result `shouldEqual` "442.77777777777777777778"
        it "divides a BigNumber  by 20 " do
            let value = "355"
                mBigNumberDivided = dividedByInt <$> (fromString value) <*> Just 5
                result = fromMaybe "0" $ toString' <$> mBigNumberDivided
            result `shouldEqual` "71"
        it "formats a BigNumber by a given format " do
            let df = unwrap defaultFormat
                newFormat = BigNumberFormat $ df { decimalSeparator = ","
                                                  , groupSeparator = "."
                                                  }
            result <- liftEff $ case fromString "123456789.1" of
                                    Nothing -> pure "0"
                                    Just bn -> toFormat bn newFormat 1
            result `shouldEqual` "123.456.789,1"
        it "formats a BigNumber by given decimal places " do
            let value = "123456789.123456789"
            result <- liftEff $ case fromString "123456789.123456789" of
                                    Nothing -> pure "0"
                                    Just bn -> toFormat bn defaultFormat 1
            result `shouldEqual` "123,456,789.1"
