module Explorer.Util.Time.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Maybe (fromMaybe)
import Data.Time.Duration (Days(Days), Milliseconds(Milliseconds), Minutes(Minutes), Seconds(Seconds))
import Data.Time.NominalDiffTime (mkTime)
import Explorer.I18n.Lang (Language(..))
import Explorer.Util.Time (prettyDuration, prettyDate)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testPrettyDuration :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testPrettyDuration =
    describe "Explorer.Util.Time.Test" do
        describe "prettyDuration" do
            describe "short durations" do
                it "a milisecond should return less than a minute" do
                  let result = prettyDuration English (Milliseconds 1.0)
                      expected = "< 1 minutes"
                  result `shouldEqual` expected
                it "a second should return less than a minute" do
                  let result = prettyDuration English (Seconds 1.0)
                      expected = "< 1 minutes"
                  result `shouldEqual` expected
                it "59 seconds should return less than a minute" do
                  let result = prettyDuration English (Seconds 59.0)
                      expected = "< 1 minutes"
                  result `shouldEqual` expected
            describe "durations" do
                it "30 minutes should return 30 minutes" do
                    let result = prettyDuration English (Minutes 30.0)
                        expected = "30 minutes"
                    result `shouldEqual` expected
            describe "longer durations" do
              it "5 days should return 5 days" do
                  let result = prettyDuration English (Days 5.0)
                      expected = "5 days"
                  result `shouldEqual` expected
        describe "prettyDate" do
              it "format date of DD.MM.YYYY HH:mm,ss" do
                  let result =  prettyDate "DD.MM.YYYY HH:mm,ss" $ mkTime 1490098347.0
                  "21.03.2017 12:12,27" `shouldEqual` fromMaybe "failed" result
              it "format date of MM/DD/YYYY HH:mm,ss" do
                  let result =  prettyDate "DD.MM.YYYY HH:mm.ss" $ mkTime 1490098689.0
                  "21.03.2017 12:18.09" `shouldEqual` fromMaybe "failed" result
