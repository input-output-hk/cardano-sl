module BigNumber.Test where

import Prelude
import BigNumber (BigNumberFormat(..), defaultFormat, dividedByInt, format, fromString, toFormat, toFormat', toString, toString')
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Test.Spec (Group, describe, it, itOnly)
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)


testBigNumber :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testBigNumber =
    describe "BigNumber" do
        it "creates an instance of a BigNumber from a string" do
            _ <- pure $ format defaultFormat
            let value = "12345678901"
                result = fromString value
            (fromMaybe "failed" $ show <$> result) `shouldEqual` value
        it "converts a BigNumber to a string by using base of 10 (default base) " do
            _ <- pure $ format defaultFormat
            let value = "750000"
                result = fromMaybe "0" $ toString' <$> fromString value
            result `shouldEqual` value
        it "converts a BigNumber to a string by using base of 9 " do
            _ <- pure $ format defaultFormat
            let value = "362.875"
                result = fromMaybe "0" $ toString <$> fromString value <*> Just 9
            result `shouldEqual` "442.77777777777777777778"
        it "divides a BigNumber  by 20 " do
            _ <- pure $ format defaultFormat
            let value = "355"
                mBigNumberDivided = dividedByInt <$> (fromString value) <*> Just 5
                result = fromMaybe "0" $ toString' <$> mBigNumberDivided
            result `shouldEqual` "71"
        it "formats a BigNumber by a given format " do
            let df = unwrap defaultFormat
                newFormat = BigNumberFormat $ df { decimalSeparator = ","
                                                  , groupSeparator = "."
                                                  }
                value = "123456789.1"
                mBigNumberFormatted = toFormat' <$> (fromString value) <*> Just newFormat <*> Just 1
                result = fromMaybe "0" (unsafeCoerce mBigNumberFormatted)
            result `shouldEqual` "123.456.789,1"
        it "formats a BigNumber by given decimal places " do
            _ <- pure $ format defaultFormat
            let value = "123456789.123456789"
                mBigNumberFormatted = toFormat <$> (fromString value) <*> Just 1
                result = fromMaybe "0" mBigNumberFormatted
            result `shouldEqual` "123,456,789.1"
