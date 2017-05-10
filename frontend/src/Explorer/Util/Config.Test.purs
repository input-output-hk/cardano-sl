module Explorer.Util.Config.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Explorer.Util.Config (SyncAction(..), syncBySocket)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)


testConfigUtil :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testConfigUtil =
    describe "Explorer.Util.Config" do
        describe "syncBySocket" do
            it "should be true" do
                (syncBySocket SyncBySocket) `shouldEqual` true
            it "should be false" do
                (syncBySocket SyncByPolling) `shouldEqual` true
