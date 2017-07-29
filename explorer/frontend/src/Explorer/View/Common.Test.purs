module Explorer.View.Common.Test where

import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Explorer.View.Common (getMaxPaginationNumber)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)

testCommonViews :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testCommonViews =
    describe "Explorer.View.Common" do

        describe "getMaxPaginationNumber" do
            it "rounds not to upper number"
                let result = getMaxPaginationNumber 530 10
                in result `shouldEqual` 53
            it "rounds to upper number"
                let result = getMaxPaginationNumber 539 10
                in result `shouldEqual` 54
