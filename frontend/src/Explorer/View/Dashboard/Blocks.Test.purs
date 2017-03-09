module Explorer.View.Dashboard.Test where

import Prelude
import Explorer.View.Dashboard.Blocks (blocksView, prettyDuration)
import Explorer.I18n.Lang (Language(..))
import Data.Time.Duration (class Duration, Milliseconds(..), Seconds(..), Minutes(..), Hours(..), Days(..), convertDuration, toDuration, fromDuration)
import Data.Int (floor, toNumber)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)

testPrettyDuration =
  describe "Explorer.View.Dashboard.Blocks.prettyDuration" do
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
