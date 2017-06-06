module Explorer.Util.String.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Either (Either(..))
import Data.Identity (Identity)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Explorer.I18n.Lang (Language(..))
import Explorer.Util.Factory (mkCoin)
import Explorer.Util.String (formatADA, substitute, parseSearchEpoch)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testStringUtil :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testStringUtil =
    describe "Explorer.Util.String" do
        describe "substitute" do
            it "takes all two arguments" do
                let result = substitute "Hello {0}, what's going on {1}?" ["Jane", "today"]
                    expected = "Hello Jane, what's going on today?"
                result `shouldEqual` expected
            it "takes the third argument only" do
                let result = substitute "Hello {2}" ["foo", "bar", "baz"]
                    expected = "Hello baz"
                result `shouldEqual` expected
            it "ignores all extra arguments" do
                let result = substitute "Hello {0}, what's going on?" ["foo", "bar", "baz"]
                    expected = "Hello foo, what's going on?"
                result `shouldEqual` expected
            it "ignores placeholder if an argument is missing" do
                let result = substitute "Hello {0}, let's go now" []
                    expected = "Hello , let's go now"
                result `shouldEqual` expected
        describe "query parser" do
            it "parses an epoch" do
                let result = Right $ Tuple (Just 3) Nothing
                    expected = parseSearchEpoch "3"
                result `shouldEqual` expected
            it "parses an epoch and raises error" do
                let result = Right $ Tuple Nothing Nothing
                    expected = parseSearchEpoch "d"
                result `shouldEqual` expected
            it "parses an epoch and a slot" do
                let result = Right $ Tuple (Just 583) (Just 12)
                    expected = parseSearchEpoch "583,12"
                result `shouldEqual` expected
            it "parses an epoch and a slot and raises error" do
                let result = Right $ Tuple (Just 583) Nothing
                    expected = parseSearchEpoch "583,aa"
                result `shouldEqual` expected
        describe "formatADA" do
            it "formats big number of lovelaces using EN format" do
                let result = formatADA (mkCoin "123456789123456789") English
                result `shouldEqual` "123,456,789,123.456789"
            it "formats zero lovelaces using EN format" do
                let result = formatADA (mkCoin "0") English
                result `shouldEqual` "0.000000"
            it "formats big number of lovelaces using DE format" do
                let result = formatADA (mkCoin "123456789123456789") German
                result `shouldEqual` "123.456.789.123,456789"
            it "formats zero lovelaces using DE format" do
                let result = formatADA (mkCoin "0") German
                result `shouldEqual` "0,000000"
            it "formats big number of lovelaces using JP format" do
                let result = formatADA (mkCoin "123456789123456789") Japanese
                result `shouldEqual` "123,456,789,123.456789"
            it "formats zero lovelaces using JP format" do
                let result = formatADA (mkCoin "0") Japanese
                result `shouldEqual` "0.000000"
