module Explorer.Util.String.Test where

import Prelude
import Explorer.Util.String (substitute)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

testStringUtil =
  describe "Explorer.Util.String" do
    describe "substitute" do
      it "takes all two arguments" do
        let result = substitute "Hello {0}, what's going on {1}?" ["Jane", "today"]
        let expected = "Hello Jane, what's going on today?"
        result `shouldEqual` expected
      it "takes the third argument only" do
        let result = substitute "Hello {2}" ["foo", "bar", "baz"]
        let expected = "Hello baz"
        result `shouldEqual` expected
      it "ignores all extra arguments" do
        let result = substitute "Hello {0}, what's going on?" ["foo", "bar", "baz"]
        let expected = "Hello foo, what's going on?"
        result `shouldEqual` expected
      it "ignores placeholder if an argument is missing" do
        let result = substitute "Hello {0}, let's go now" []
        let expected = "Hello , let's go now"
        result `shouldEqual` expected
