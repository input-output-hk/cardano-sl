module Explorer.Util.Time.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Time.Duration (Days(Days), Milliseconds(Milliseconds), Minutes(Minutes), Seconds(Seconds))
import Explorer.I18n.Lang (Language(..))
import Explorer.Util.Time (prettyDuration)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testPrettyDuration :: forall eff. StateT (Array (Group (Aff eff Unit))) Identity Unit
testPrettyDuration =
  describe "Explorer.Util.Time.Test.prettyDuration" do
    describe "short durations" do
      it "a milisecond should return less than a minute" do
        let result = prettyDuration English (Milliseconds 1.0)
        let expected = "< 1 Minutes"
        result `shouldEqual` expected
      it "a second should return less than a minute" do
        let result = prettyDuration English (Seconds 1.0)
        let expected = "< 1 Minutes"
        result `shouldEqual` expected
      it "59 seconds should return less than a minute" do
        let result = prettyDuration English (Seconds 59.0)
        let expected = "< 1 Minutes"
        result `shouldEqual` expected
    describe "durations" do
      it "30 minutes should return 30 minutes" do
        let result = prettyDuration English (Minutes 30.0)
        let expected = "30 Minutes"
        result `shouldEqual` expected
    describe "longer durations" do
      it "5 days should return 5 days" do
        let result = prettyDuration English (Days 5.0)
        let expected = "5 Days"
        result `shouldEqual` expected
