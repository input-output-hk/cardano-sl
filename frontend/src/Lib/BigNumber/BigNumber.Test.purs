module BigNumber.Test where

import Prelude
import BigNumber (fromString)
import Control.Monad.Aff (Aff)
import Control.Monad.State (StateT)
import Data.Identity (Identity)
import Data.Maybe (fromMaybe)
import Test.Spec (Group, describe, it)
import Test.Spec.Assertions (shouldEqual)


testBigNumber :: forall r. StateT (Array (Group (Aff r Unit))) Identity Unit
testBigNumber =
    describe "BigNumber" do
        describe "creates an instance" do
            it "from a string"
                let value = "123456789012345678901234567890"
                    result = fromString "123456789012345678901234567890"
                in
                  (fromMaybe "failed" $ show <$> result) `shouldEqual` value
