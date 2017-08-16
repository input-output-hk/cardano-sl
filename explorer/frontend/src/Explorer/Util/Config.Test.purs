module Explorer.Util.Config.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Explorer.Util.Config (SyncAction(..), isTestnet, syncByPolling, syncBySocket)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)


testConfigUtil :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testConfigUtil =
    describe "Explorer.Util.Config" do
        describe "syncBySocket" do
            it "should be true" do
                (syncBySocket SyncBySocket) `shouldEqual` true
            it "should be false" do
                (syncBySocket SyncByPolling) `shouldEqual` false
        describe "syncByPolling" do
            it "should be true" do
                (syncByPolling SyncByPolling) `shouldEqual` true
            it "should be false" do
                (syncByPolling SyncBySocket) `shouldEqual` false
        describe "isTestnet" do
            it "should be true" do
                (isTestnet "https://www.testnet.cardanoexplorer.com/") `shouldEqual` true
            it "should be false" do
                (isTestnet "https://www.cardanoexplorer.com") `shouldEqual` false
