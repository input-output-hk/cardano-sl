module Explorer.Routes.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Explorer.Routes (Route(..), match, paramToString, toUrl)
import Explorer.Util.Factory (mkCAddress, mkCHash, mkCTxId, mkEpochIndex, mkLocalSlotIndex)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testRoutes :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testRoutes =
    describe "Explorer.Routes" do
        describe "type class RouteParams" do
            it "stringified param of an EpochIndex"
              let index =  mkEpochIndex 101
                  result = paramToString index
              in result `shouldEqual` "101"
            it "stringified param of an SlotIndex"
              let index =  mkLocalSlotIndex 102
                  result = paramToString index
              in result `shouldEqual` "102"
        describe "toUrl" do
            it "parses url of Dashboard"
              let result = toUrl Dashboard
              in result `shouldEqual` "/"
            it "parses url of Tx"
              let result = toUrl <<< Tx $ mkCTxId "1"
              in result `shouldEqual` "/tx/1"
            it "parses url of Address"
              let result = toUrl <<< Address $ mkCAddress "1"
              in result `shouldEqual` "/address/1"
            it "parses url of Epoch"
              let result = toUrl <<< Epoch $ mkEpochIndex 1
              in result `shouldEqual` "/epoch/1"
            it "parses url of EpochSlot"
              let result = toUrl $ EpochSlot (mkEpochIndex 1) (mkLocalSlotIndex 2)
              in result `shouldEqual` "/epoch/1/slot/2"
            it "parses url of Calculator"
              let result = toUrl Calculator
              in result `shouldEqual` "/calculator"
            it "parses url of Block"
              let result = toUrl <<< Block $ mkCHash "1"
              in result `shouldEqual` "/slot/1"
            it "parses url of NotFound"
              let result = toUrl NotFound
              in result `shouldEqual` "/404"
        describe "match" do
            it "Dashboard"
              let url = "/"
                  result = match url
              in toUrl result `shouldEqual` url
            it "Tx"
              let url = "/tx/1"
                  result = match url
              in toUrl result `shouldEqual` url
            it "Address"
              let url = "/address/1"
                  result = match url
              in toUrl result `shouldEqual` url
            it "Epoch"
              let url = "/epoch/1"
                  result = match url
              in toUrl result `shouldEqual` url
            it "EpochSlot"
              let url = "/epoch/1/slot/2"
                  result = match url
              in toUrl result `shouldEqual` url
            it "Calculator"
              let url = "/calculator"
                  result = match url
              in (toUrl result) `shouldEqual` url
            it "Block"
              let url = "/slot/1"
                  result = match url
              in toUrl result `shouldEqual` url
            it "NotFound"
              let url = "/xyz"
                  result = match url
              in toUrl result `shouldEqual` "/404"
